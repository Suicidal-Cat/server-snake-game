(ns server.game.cake-game
  (:require
   [server.db.dbBroker :as db :refer [get-random-cake-with-parts]]
   [server.game.game-helper-func :refer [close-sockets
                                         generate-valid-coordinate-pair-ball
                                         init-game-cake-state
                                         move-snake-borderless send-snake-data
                                         update-player-direction
                                         vector-contains?]]))

(def field-size 594) ;field size in px
(def grid-size 27) ;grid size in px
(def tick-duration 120)

;snakes-direction hash-map - (:snake1 plyer1 :snake2 player2)
;player - hash-map (:socket socket :direction direction)
;online-games - hash map :gameId snakes-direction
(def online-games (atom {}))

;stop the game and save the result
(defn end-game-loop [stop-flag final-score result]
  (reset! final-score result)
  (reset! stop-flag true)
  (db/save-game @final-score true (:cake db/game-mode-enum)))

;check snake collisions
(defn snake-collisions [game-state stop-game final-score player1 player2]
  (let [snake1 (:snake1 @game-state)
        snake2 (:snake2 @game-state)]
    (if (or
         (vector-contains? snake2 (snake1 0))
         (vector-contains? (subvec snake1 1) (snake1 0)))
      (end-game-loop stop-game final-score {:winner {:id (:id player2) :head ((:snake1 @game-state) 0) :cake (:cake2 @game-state)}
                                            :loser {:id (:id player1) :head ((:snake2 @game-state) 0) :cake (:cake1 @game-state)}})
      (when (or
             (vector-contains? snake1 (snake2 0))
             (vector-contains? (subvec snake2 1) (snake2 0)))
        (end-game-loop stop-game final-score {:winner {:id (:id player1) :head ((:snake1 @game-state) 0) :cake (:cake1 @game-state)}
                                              :loser {:id (:id player2) :head ((:snake2 @game-state) 0) :cake (:cake2 @game-state)}})))))

;generate random cake part
(defn generate-random-part [game-state]
  (let [coordinate (generate-valid-coordinate-pair-ball field-size grid-size
                                                        (:snake1 game-state) (:snake2 game-state)
                                                        :offset 0 :exclude-edges false :blocked-fields (:parts game-state))
        parts1 (filterv #(<= (:current %) (:amount %)) (:parts (:cake1 game-state))) n1 (count parts1)
        parts2 (filterv #(<= (:current %) (:amount %)) (:parts (:cake2 game-state))) n2 (count parts2)
        idx (rand-int (+ n1 n2))
        part (if (< idx n1)
               (nth parts1 idx)
               (nth parts2 (- idx n1)))]
    (hash-map :image (:part-image part) :coordinate coordinate :part-id (:part-id part))))

;generate part on the field
(defn generate-cake-parts [game-state stop-game]
  (future
    (while (not @stop-game)
      (let [cake (generate-random-part @game-state)
            duration 2500]
        (Thread/sleep duration)
        (swap! game-state (fn [game-state] (assoc game-state :parts (conj (:parts game-state) cake))))))))

;check if snake have eaten a part
(defn get-eaten-part [head parts]
  (some #(when (= head (:coordinate %)) %) parts))

;check if players have collected all parts
(defn check-if-snake-have-eaten-cake [game-state player1 player2 stop-game final-score]
  (if (every? #(and (> (:current %) 0) (>= (:current %) (:amount %))) (get-in @game-state [:cake1 :parts]))
    (end-game-loop stop-game final-score {:winner {:id (:id player1) :head ((:snake1 @game-state) 0) :cake (:cake1 @game-state)}
                                          :loser {:id (:id player2) :head ((:snake2 @game-state) 0) :cake (:cake2 @game-state)}})
    (when (every? #(and (> (:current %) 0) (>= (:current %) (:amount %))) (get-in @game-state [:cake2 :parts]))
      (end-game-loop stop-game final-score {:winner {:id (:id player2) :head ((:snake1 @game-state) 0) :cake (:cake2 @game-state)}
                                            :loser {:id (:id player1) :head ((:snake2 @game-state) 0) :cake (:cake1 @game-state)}}))))

;update current amount of the part in the cake
(defn update-part-current [parts part-id]
  (loop [remaining parts
         acc []
         updated? false]
    (cond
      (empty? remaining) (vec acc)
      updated? (into acc remaining)
      :else (let [p (first remaining)]
              (if (= (:part-id p) part-id)
                (recur (subvec remaining 1) (conj acc (update p :current inc)) true)
                (recur (subvec remaining 1) (conj acc p) false))))))

;update game when snake eat a cake part
(defn update-game-on-eat [game-state player1 player2 stop-game final-score]
  (let [[head-s1 & _] (:snake1 @game-state)
        [head-s2 & _] (:snake2 @game-state)
        part1 (get-eaten-part head-s1 (:parts @game-state))
        part2 (get-eaten-part head-s2 (:parts @game-state))]

    (when (or part1 part2)
      (swap! game-state
             (fn [state]
               (let [state (cond-> state
                             true (update :parts #(remove (fn [p]
                                                            (or (and part1 (= (:coordinate p) (:coordinate part1)))
                                                                (and part2 (= (:coordinate p) (:coordinate part2)))))
                                                          %))

                             part1 (-> (update :snake1 conj [-1 -1])
                                       (update :cake1
                                               #(update-in % [:parts]
                                                           (fn [parts] (update-part-current parts (:part-id part1))))))

                             part2 (-> (update :snake2 conj [-1 -1])
                                       (update :cake2
                                               #(update-in % [:parts]
                                                           (fn [parts] (update-part-current parts (:part-id part2)))))))]
                 state)))
      (check-if-snake-have-eaten-cake game-state player1 player2 stop-game final-score))))

;update snakes positions
(defn update-snakes-positions [game-state snake-directions]
  (assoc game-state
         :snake1 (:snake1 game-state)
         :snake2 (move-snake-borderless (:snake2 game-state) (:direction (:snake2 @snake-directions)) grid-size field-size)))

;; update initial game state
(defn init-parts [game-state]
  (swap! game-state
         (fn [state]
           (let [part1 (generate-random-part state)
                 state' (update state :parts conj part1)
                 part2 (generate-random-part state')]
             (assoc state :parts [part1 part2])))))

;game loop
(defn broadcast-game-state [player1 player2 game-id]
  (future
    (let [game-state (atom (assoc (init-game-cake-state game-id)
                                  :cake1 (get-random-cake-with-parts)
                                  :cake2 (get-random-cake-with-parts)
                                  :parts []))
          snake-directions ((keyword game-id) @online-games)
          stop-game (atom false)
          final-score (atom nil)]
      (init-parts game-state)
      (send-snake-data player1 player2 (assoc @game-state :snake1-id (:id player1)))
      (Thread/sleep 3000)
      (generate-cake-parts game-state stop-game)
      (while (not @stop-game)
        (Thread/sleep tick-duration)
        (send-snake-data player1 player2 @game-state)
        (update-game-on-eat game-state player1 player2 stop-game final-score)
        (swap! game-state update-snakes-positions snake-directions)
        (snake-collisions game-state stop-game final-score player1 player2)
        (swap! snake-directions (fn [state] (assoc-in (assoc-in state [:snake1 :change-dir] true) [:snake2 :change-dir] true))))
      (Thread/sleep 50)
      (send-snake-data player1 player2 @final-score)
      (swap! online-games dissoc (keyword game-id))
      (close-sockets player1 player2))))

;start the game
(defn start-cake-game [player1 player2]
  (let [game-id (str (:id player1) (:id player2))
        snakes-direction (atom {:snake1 (assoc player1 :direction :right :change-dir true)
                                :snake2 (assoc player2 :direction :left :change-dir true)})]
    (swap! online-games assoc (keyword game-id) snakes-direction)
    (broadcast-game-state player1 player2 game-id)))

;update snake direction
(defn change-direction [player-socket dir]
  (update-player-direction player-socket dir online-games))