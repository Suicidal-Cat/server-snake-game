(ns server.game.main-game
  (:require
   [server.db.dbBroker :as db]
   [server.game.game-helper-func :refer [close-sockets
                                         generate-valid-coordinate-pair-ball
                                         in-bounds? init-game-state inside?
                                         move-snake send-snake-data
                                         update-player-direction
                                         vector-contains?]]))

(def field-size 594) ;field size in px
(def grid-size 27) ;grid size in px
(def power-ups ["+3","-3","boom"])
(def tick-duration 120)

;snakes-direction hash-map - (:snake1 plyer1 :snake2 player2)
;player - hash-map (:socket socket :direction direction)
;online-games - hash map :gameId snakes-direction
(def online-games (atom {}))

;stop the game and save the result
(defn end-game-loop [stop-flag final-score result]
  (reset! final-score result)
  (reset! stop-flag true)
  (db/save-game @final-score true (:time db/game-mode-enum)))

;check snake collisions
(defn snake-collisions [game-state stop-game final-score player1 player2]
  (let [snake1 (:snake1 @game-state)
        snake2 (:snake2 @game-state)]
    (if (or
         (false? (in-bounds? (snake1 0) field-size grid-size))
         (vector-contains? snake2 (snake1 0))
         (vector-contains? (subvec snake1 1) (snake1 0)))
      (end-game-loop stop-game final-score {:winner {:id (:id player2) :score ((:score @game-state) 1) :head (snake2 0)}
                                            :loser {:id (:id player1) :score ((:score @game-state) 0) :head (snake1 0)}})
      (when (or
             (false? (in-bounds? (snake2 0) field-size grid-size))
             (vector-contains? snake1 (snake2 0))
             (vector-contains? (subvec snake2 1) (snake2 0)))
        (end-game-loop stop-game final-score {:winner {:id (:id player1) :score ((:score @game-state) 0) :head (snake1 0)}
                                              :loser {:id (:id player2) :score ((:score @game-state) 1) :head (snake2 0)}})))))

;grow snake when it eats the ball and generate new ball
(defn update-game-on-eat [game-state]
  (let [[head-s1 & _] (:snake1 @game-state)
        [head-s2 & _] (:snake2 @game-state)
        fixed-ball (mapv #(- % (/ grid-size 2)) (:ball @game-state))]
    (if (= fixed-ball head-s1)
      (swap! game-state (fn [game-state] (assoc game-state
                                                :snake1 (conj (:snake1 game-state) [-1 -1])
                                                :ball (generate-valid-coordinate-pair-ball field-size grid-size (:snake1 game-state) (:snake2 game-state))
                                                :score [(inc ((:score game-state) 0)) ((:score game-state) 1)])))
      (when (= fixed-ball head-s2)
        (swap! game-state (fn [game-state] (assoc game-state
                                                  :snake2 (conj (:snake2 game-state) [-1 -1])
                                                  :ball (generate-valid-coordinate-pair-ball field-size grid-size (:snake1 game-state) (:snake2 game-state))
                                                  :score [((:score game-state) 0) (inc ((:score game-state) 1))])))))))

;generate power on the field
(defn generate-random-power [game-state stop-game power-ups]
  (future
    (while (not @stop-game)
      (Thread/sleep (+ 6000 (rand-int 3001)))
      (let [power (rand-nth power-ups)
            cordinates (generate-valid-coordinate-pair-ball field-size grid-size (:snake1 @game-state) (:snake2 @game-state))
            duration 5000
            hehe (if (and (not= power "boom") (= (rand-int (count power-ups)) 1)) true false)]
        (if hehe
          (swap! game-state (fn [game-state] (assoc game-state :power {:value power :cord cordinates :random true})))
          (swap! game-state (fn [game-state] (assoc game-state :power {:value power :cord cordinates}))))
        (Thread/sleep duration)
        (if (= power "boom")
          (swap! game-state (fn [game-state] (assoc-in game-state [:power, :value] "boomed")))
          (swap! game-state (fn [game-state] (assoc game-state :power nil))))))))

;update game-state based on power
(defn update-power-consumed [game-state sn-consum sn-opp power-val]
  (let [sn-consum-v (sn-consum game-state)
        sn-consum-size (count sn-consum-v)]
    (case power-val
      "+3" (assoc game-state sn-opp (conj (sn-opp game-state) [-1 -1] [-1 -1] [-1 -1]) :power nil)
      "-3" (if (> sn-consum-size 5) (assoc game-state sn-consum (subvec sn-consum-v 0 (- sn-consum-size 3)) :power nil) game-state)
      "boom" (assoc game-state :lost sn-consum))))

;update game on boomed
(defn update-game-boomed [game-state final-score stop-game player1 player2 snh1 snh2 cord]
  (let [x (- (cord 0) (* grid-size 3/2))
        y (- (cord 1) (* grid-size 3/2))
        size (* grid-size 3)
        i1 (inside? snh1 x y size size)
        i2 (inside? snh2 x y size size)]
    (if (and i1 i2)
      (end-game-loop stop-game final-score {:winner {:id (:id player2) :score ((:score @game-state) 1) :head snh2}
                                            :loser {:id (:id player1) :score ((:score @game-state) 0) :head snh1}
                                            :draw true})
      (if i1 (end-game-loop stop-game final-score {:winner {:id (:id player2) :score ((:score @game-state) 1) :head snh2}
                                                   :loser {:id (:id player1) :score ((:score @game-state) 0) :head snh1}})
          (if i2 (end-game-loop stop-game final-score {:winner {:id (:id player1) :score ((:score @game-state) 0) :head snh1}
                                                         :loser {:id (:id player2) :score ((:score @game-state) 1) :head snh2}})
              (swap! game-state (fn [game-state] (assoc game-state :power nil))))))))

;update game on consumed power
(defn update-game-on-power [game-state final-score stop-game player1 player2]
  (when-let [power (:power @game-state)]
    (let [[head-s1 & _] (:snake1 @game-state)
          [head-s2 & _] (:snake2 @game-state)
          power-cord (mapv #(- % (/ grid-size 2)) (:cord power))]
      (if (= (:value power) "boomed")
        (update-game-boomed game-state final-score stop-game player1 player2 head-s1 head-s2 power-cord)
        (if (= power-cord head-s1)
          (do (swap! game-state (fn [game-state] (update-power-consumed game-state :snake1 :snake2 (:value power))))
              (when (:lost game-state)
                (end-game-loop stop-game final-score {:winner {:id (:id player2) :score ((:score @game-state) 1) :head head-s2}
                                                      :loser {:id (:id player1) :score ((:score @game-state) 0) :head head-s1}})))
          (when (= power-cord head-s2)
            (swap! game-state (fn [game-state] (update-power-consumed game-state :snake2 :snake1 (:value power))))
            (when (:lost @game-state)
              (end-game-loop stop-game final-score {:winner {:id (:id player1) :score ((:score @game-state) 0) :head head-s1}
                                                    :loser {:id (:id player2) :score ((:score @game-state) 1) :head head-s2}}))))))))

;update snakes positions
(defn update-snakes-positions [game-state snake-directions]
  (assoc game-state
         :snake1 (:snake1 game-state)
         :snake2 (move-snake (:snake2 game-state) (:direction (:snake2 @snake-directions)) grid-size)))

(defn update-clock-time [game-state final-score stop-game player1 player2]
  (swap! game-state update :time-left #(max 0 (- % tick-duration)))
  (when (zero? (:time-left @game-state))
    (let [[score1 score2] (:score @game-state)
          [head-s1 & _] (:snake1 @game-state)
          [head-s2 & _] (:snake2 @game-state)]
      (cond
        (> score1 score2) (end-game-loop stop-game final-score {:winner {:id (:id player1) :score ((:score @game-state) 0) :head head-s1}
                                                                :loser {:id (:id player2) :score ((:score @game-state) 1) :head head-s2}})
        (< score1 score2) (end-game-loop stop-game final-score {:winner {:id (:id player2) :score ((:score @game-state) 1) :head head-s2}
                                                                :loser {:id (:id player1) :score ((:score @game-state) 0) :head head-s1}})
        :else             (end-game-loop stop-game final-score {:winner {:id (:id player2) :score ((:score @game-state) 1) :head head-s1}
                                                                :loser {:id (:id player1) :score ((:score @game-state) 0) :head head-s2}
 
                                                                :draw true})))))
;game loop
(defn broadcast-game-state [player1 player2 game-id]
  (future
    (let [game-state (atom (assoc (init-game-state field-size grid-size game-id)
                                  :time-left (* 2 60 1000)))
          snake-directions ((keyword game-id) @online-games)
          stop-game (atom false)
          final-score (atom nil)] 
      (send-snake-data player1 player2 (assoc @game-state :snake1-id (:id player1)))
      (Thread/sleep 3000)
      (generate-random-power game-state stop-game power-ups)
      (while (not @stop-game)
        (Thread/sleep tick-duration)
        (send-snake-data player1 player2 @game-state)
        (update-game-on-eat game-state)
        (update-game-on-power game-state final-score stop-game player1 player2)
        (swap! game-state update-snakes-positions snake-directions)
        (snake-collisions game-state stop-game final-score player1 player2)
        (update-clock-time game-state final-score stop-game player1 player2)
        (swap! snake-directions (fn [state] (assoc-in (assoc-in state [:snake1 :change-dir] true) [:snake2 :change-dir] true))))
      (send-snake-data player1 player2 @final-score) 
      (swap! online-games dissoc (keyword game-id))
      (Thread/sleep 100)
      (close-sockets player1 player2))))

;start the game
(defn start-main-game [player1 player2]
  (let [game-id (str (:id player1) (:id player2))
        snakes-direction (atom {:snake1 (assoc player1 :direction :right :change-dir true)
                                :snake2 (assoc player2 :direction :left :change-dir true)})]
    (swap! online-games assoc (keyword game-id) snakes-direction)
    (broadcast-game-state player1 player2 game-id)))

;update snake direction
(defn change-direction [player-socket dir]
  (update-player-direction player-socket dir online-games))