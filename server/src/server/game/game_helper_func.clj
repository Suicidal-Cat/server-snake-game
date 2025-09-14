(ns server.game.game-helper-func
  (:require [ring.websocket :as ws]))

;helper functions

;check if snake part is in playing field
(defn in-bounds? [[x y] field-size grid-size]
  (and (>= x grid-size)
       (< x (- field-size grid-size))
       (>= y grid-size)
       (< y (- field-size grid-size))))

;check if coordinates are inside given field
(defn inside? [[x y] x-rec y-rec h-rec w-rec]
  (and (>= x x-rec)
       (< x (+ x-rec w-rec))
       (>= y y-rec)
       (< y (+ y-rec h-rec))))

;; gets random coordinate depending on exlude-edges flag
(defn random-coordinate
  [field-size grid-size & {:keys [exclude-edges] :or {exclude-edges true}}]
  (let [usable-size (if exclude-edges
                      (- field-size (* 2 grid-size))
                      field-size)
        num-cells (/ usable-size grid-size)
        base (if exclude-edges grid-size 0)]
    (+ base (* grid-size (rand-int num-cells)))))

(defn generate-valid-coordinate-pair-ball
  [field-size grid-size sn1 sn2
   & {:keys [offset exclude-edges blocked-fields]
      :or {offset (/ grid-size 2) exclude-edges true blocked-fields []}}]
  (let [blocked (set (concat sn1 sn2 blocked-fields))
        safe-area (* grid-size 2)]
    (loop []
      (let [x (random-coordinate field-size grid-size :exclude-edges exclude-edges)
            y (random-coordinate field-size grid-size :exclude-edges exclude-edges)
            coordinate [x y]]
        (if (or (contains? blocked coordinate)
                (inside? coordinate (- ((sn1 0) 0) safe-area) (- ((sn1 0) 1) safe-area) (* safe-area 2) (* safe-area 2))
                (if sn2 (inside? coordinate (- ((sn2 0) 0) safe-area) (- ((sn2 0) 1) safe-area) (* safe-area 2) (* safe-area 2)) false))
          (recur)
          (mapv #(+ offset %) coordinate))))))

;init main game-state
(defn init-game-state [field-size grid-size game-id]
  (let [snake1 [[162 108] [135 108] [108 108] [81 108]]
        snake2 [[405 459] [432 459] [459 459] [486 459]]
        ball (generate-valid-coordinate-pair-ball field-size grid-size
                                                  snake1
                                                  snake2)]
    (hash-map :snake1 snake1
              :snake2 snake2
              :ball ball
              :score [0 0]
              :game-id game-id)))

;init singleplayer game state
(defn game-state-single [field-size grid-size game-id]
  (let [snake [[198 99] [165 99] [132 99] [99 99]]
        ball (generate-valid-coordinate-pair-ball field-size grid-size
                                                  snake
                                                  nil
                                                  :offset 0)]
    (hash-map :snake1 snake
              :ball ball
              :score [0]
              :game-id game-id)))

;init main game-state
(defn init-game-cake-state [game-id]
  (let [snake1 [[162 108] [135 108] [108 108] [81 108]]
        snake2 [[405 459] [432 459] [459 459] [486 459]]]
    (hash-map :snake1 snake1
              :snake2 snake2
              :game-id game-id)))

;check if vector contains element
(defn vector-contains? [v el]
  (some #(= % el) v))

;find game by player's socket
(defn find-players-by-socket [socket online-games]
  (some (fn [[_ players]]
          (when (some #(= socket (:socket (val %))) @players)
            players))
        online-games))

;update snake position with border
(defn move-snake [snake direction speed]
  (let [[x y] (snake 0)
        new-head (case direction
                   :up    [x (- y speed)]
                   :down  [x (+ y speed)]
                   :left  [(- x speed) y]
                   :right [(+ x speed) y])]
    (into [new-head] (subvec snake 0 (dec (count snake))))))

;update snake position without border
(defn move-snake-borderless [snake direction speed field-size]
  (let [[x y] (snake 0)
        new-head (case direction
                   :up    [x (mod (- y speed) field-size)]
                   :down  [x (mod (+ y speed) field-size)]
                   :left  [(mod (- x speed) field-size) y]
                   :right [(mod (+ x speed) field-size) y])]
    (into [new-head] (subvec snake 0 (dec (count snake))))))

;update player direction
(defn update-player-direction [player-socket dir online-games]
  (let [snakes-direction (find-players-by-socket player-socket @online-games)
        [snake player] (if (= player-socket (:socket (:snake1 @snakes-direction)))
                         [:snake1 (:snake1 @snakes-direction)]
                         [:snake2 (:snake2 @snakes-direction)])
        past-dir (:direction player)
        update-dir (fn [] (swap! snakes-direction update snake (fn [_] (assoc player :direction dir :change-dir false))))]
    (when (and (:change-dir (snake @snakes-direction)) (not= past-dir dir))
      (cond
        (and (= past-dir :up) (not= dir :down)) (update-dir)
        (and (= past-dir :down) (not= dir :up)) (update-dir)
        (and (= past-dir :left) (not= dir :right)) (update-dir)
        (and (= past-dir :right) (not= dir :left)) (update-dir)))))

;; send snake data
(defn send-snake-data [player1 player2 game-state]
  (ws/send (:socket player1) (pr-str game-state))
  (ws/send (:socket player2) (pr-str game-state)))

;; send snake data
(defn close-sockets [player1 player2]
  (ws/close (:socket player1))
  (ws/close (:socket player2)))