(ns server.game.singleplayer-game
  (:require
   [ring.websocket :as ws]
   [server.db.dbBroker :as db]
   [server.game.game-helper-func :refer [find-players-by-socket
                                         game-state-single
                                         generate-valid-coordinate-pair-ball
                                         move-snake-borderless
                                         vector-contains?]]))

(def field-size 594) ;field size in px
(def grid-size 33) ;grid size in px

;snakes-direction hash-map - (:snake1 plyer1)
;player - hash-map (:socket socket :direction direction)
;online-games - hash map :gameId snakes-direction
(def online-games (atom {}))

;stop the game and save the result
(defn end-game-loop [stop-flag final-score result]
  (reset! final-score result)
  (reset! stop-flag true))

;check snake collisions
(defn snake-collisions [game-state stop-game final-score player1]
  (let [snake1 (:snake1 @game-state)]
    (when (vector-contains? (subvec snake1 1) (snake1 0))
      (end-game-loop stop-game final-score {:winner {:id (:id player1) :score ((:score @game-state) 0) :head (snake1 0)}}))))

;grow snake when it eats the ball and generate new ball
(defn update-game-on-eat [game-state]
  (let [[head-s1 & _] (:snake1 @game-state)
        ball (:ball @game-state)]
    (when (= ball head-s1)
      (swap! game-state (fn [game-state] (assoc game-state
                                                :snake1 (conj (:snake1 game-state) [-1 -1])
                                                :ball (generate-valid-coordinate-pair-ball field-size grid-size (:snake1 game-state) nil :offset 0)
                                                :score [(inc ((:score game-state) 0))]))))))

(defn broadcast-game-state [player1 game-id]
  (future
    (let [game-state (atom (game-state-single field-size grid-size game-id))
          snake-directions ((keyword game-id) @online-games)
          stop-game (atom false)
          final-score (atom nil)]
      (ws/send (:socket player1) (pr-str @game-state))
      (Thread/sleep 1000)
      (while (not @stop-game)
        (Thread/sleep 110)
        (update-game-on-eat game-state)
        (swap! game-state (fn [game-state]
                            (assoc game-state
                                   :snake1 (move-snake-borderless (:snake1 game-state) (:direction (:snake1 @snake-directions)) grid-size field-size))))
        (snake-collisions game-state stop-game final-score player1) 
        (ws/send (:socket player1) (pr-str @game-state))
        (swap! snake-directions (fn [state] (assoc-in state [:snake1 :change-dir] true)))) 
      (Thread/sleep 100)
      (ws/send (:socket player1) (pr-str @final-score))
      (swap! online-games dissoc (keyword game-id)) 
      (ws/close (:socket player1)))))

;start the game
(defn start-game-single [player1]
  (let [game-id (str (:id player1))
        snakes-direction (atom {:snake1 (assoc player1 :direction :right :change-dir true)})]
    (swap! online-games assoc (keyword game-id) snakes-direction)
    (broadcast-game-state player1 game-id)))

(defn change-direction-single [player-socket dir]
  (let [snakes-direction (find-players-by-socket player-socket @online-games)
        [snake player] [:snake1 (:snake1 @snakes-direction)]
        past-dir (:direction player)
        update-dir (fn [] (swap! snakes-direction update snake (fn [_] (assoc player :direction dir :change-dir false))))]
    (when (:change-dir (snake @snakes-direction))
      (if (not= past-dir dir)
        (cond
          (and (= past-dir :up) (not= dir :down)) (update-dir)
          (and (= past-dir :down) (not= dir :up)) (update-dir)
          (and (= past-dir :left) (not= dir :right)) (update-dir)
          (and (= past-dir :right) (not= dir :left)) (update-dir))
        nil))))