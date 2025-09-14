(ns server.core-test
   (:require
    [midje.sweet :refer [=> fact facts]]
    [server.core :refer :all]
    [server.game.main-game :as main]
    [server.game.singleplayer-game :as single]
    [server.game.cake-game :as cake]
    [server.game.game-helper-func :refer :all]))

 (fact "in-bounds? checks if a position is within the playing field"
       (in-bounds? [50 50] 200 20) => true
       (in-bounds? [10 50] 200 20) => false
       (in-bounds? [190 50] 200 20) => false
       (in-bounds? [50 10] 200 20) => false
       (in-bounds? [50 190] 200 20) => false)

 (facts "inside? check if cords are inside rectangle"
        (fact "coordinates are inside the rectangle"
              (inside? [5 5] 0 0 10 10) => true
              (inside? [2 3] 1 1 5 5) => true
              (inside? [9 9] 0 0 10 10) => true
              (inside? [5 0] 0 0 10 10) => true)

        (fact "coordinates are outside the rectangle"
              (inside? [15 15] 0 0 10 10) => false
              (inside? [0 0] 1 1 5 5) => false
              (inside? [10 10] 0 0 10 10) => false
              (inside? [10 5] 0 0 10 10) => false))

 (fact "vector-contains? checks if an element is in a vector"
       (vector-contains? [1 2 3] 2) => true
       (vector-contains? [1 2 3] 4) => nil
       (vector-contains? [] 1) => nil)

 (facts "Testing random-coordinate"
        (let [field-size 100
              grid-size 10]

          (let [x (random-coordinate field-size grid-size :exclude-edges true)]
            (fact "coordinate is within bounds excluding edges"
                  (and (>= x grid-size)
                       (< x (- field-size grid-size))
                       (zero? (mod x grid-size))) => true))

          (let [x (random-coordinate field-size grid-size :exclude-edges false)]
            (fact "coordinate is within bounds including edges"
                  (and (>= x 0)
                       (< x field-size)
                       (zero? (mod x grid-size))) => true))))

 (facts "Testing generate-valid-coordinate-pair-ball"
        (let [field-size 500
              grid-size 20
              snake1 [[240 120] [260 120]]
              snake2 [[100 200] [120 200]]
              offset (/ grid-size 2)]

          (let [[x y] (generate-valid-coordinate-pair-ball field-size grid-size snake1 snake2)
                coordinate [x y]]
            (fact "coordinate is in bounds"
                  (in-bounds? coordinate field-size grid-size) => true)
            (fact "coordinate not inside snake1"
                  (vector-contains? snake1 (mapv - coordinate [offset offset])) => nil)
            (fact "coordinate not inside snake2"
                  (vector-contains? snake2 (mapv - coordinate [offset offset])) => nil))

          (let [blocked [[300 300]]
                [x y] (generate-valid-coordinate-pair-ball
                       field-size grid-size snake1 snake2
                       :offset 0
                       :exclude-edges false
                       :blocked-fields blocked)
                coordinate [x y]]
            (fact "coordinate not inside snake1"
                  (vector-contains? snake1 coordinate) => nil)
            (fact "coordinate not inside snake2"
                  (vector-contains? snake2 coordinate) => nil)
            (fact "coordinate not in blocked-fields"
                  (not (contains? (set blocked) coordinate)) => true))))

 (facts "Testing find-players-by-socket"
        (let [game1 {:player1 {:socket "socket1"} :player2 {:socket "socket2"}}
              game2 {:player1 {:socket "socket3"}}
              online-games {:1 (atom game1)
                            :2 (atom game2)}]
          (fact "Should find players by matching socket"
                (deref (find-players-by-socket "socket1" online-games)) => {:player1 {:socket "socket1"}
                                                                            :player2 {:socket "socket2"}})
          (fact "Should find players for another matching socket"
                (deref (find-players-by-socket "socket3" online-games)) => {:player1 {:socket "socket3"}})
          (fact "Should return nil for a socket that doesn't exist"
                (find-players-by-socket "socket6" online-games) => nil)))

 (facts "Move snake"
        (move-snake [[50 50] [50 60] [50 70]] :up 10) => [[50 40] [50 50] [50 60]]
        (move-snake [[50 50] [50 60] [50 70]] :down 10) => [[50 60] [50 50] [50 60]]
        (move-snake [[50 50] [50 60] [50 70]] :left 10) => [[40 50] [50 50] [50 60]]
        (move-snake [[50 50] [50 60] [50 70]] :right 10) => [[60 50] [50 50] [50 60]]
        (move-snake-borderless [[5 5] [5 6] [5 7]] :up 1 10) => [[5 4] [5 5] [5 6]]
        (move-snake-borderless [[5 5] [5 4] [5 3]] :down 1 10) => [[5 6] [5 5] [5 4]]
        (move-snake-borderless [[5 5] [6 5] [7 5]] :left 1 10) => [[4 5] [5 5] [6 5]]
        (move-snake-borderless [[5 5] [4 5] [3 5]] :right 1 10) => [[6 5] [5 5] [4 5]]
        (move-snake-borderless [[5 0] [5 9] [5 8]] :up 1 10) => [[5 9] [5 0] [5 9]]
        (move-snake-borderless [[5 9] [5 8] [5 7]] :down 1 10) => [[5 0] [5 9] [5 8]]
        (move-snake-borderless [[0 5] [9 5] [8 5]] :left 1 10) => [[9 5] [0 5] [9 5]]
        (move-snake-borderless [[9 5] [8 5] [7 5]] :right 1 10) => [[0 5] [9 5] [8 5]])

 (facts "On eat snake"
        (let [game-state (atom {:snake1 [[162 108] [135 108] [108 108] [81 108]]
                                :snake2 [[405 486] [432 486] [459 486] [486 486]]
                                :ball [351/2 243/2]
                                :score [0 0]})]
          (main/update-game-on-eat game-state)
          (fact (count (:snake1 @game-state)) => 5)
          (fact (not= (:ball @game-state) [162 108]) => true)
          (fact (:score @game-state) => [1 0])
          (swap! game-state (fn [game-state] (assoc game-state :ball [837/2 999/2])))
          (main/update-game-on-eat game-state)
          (fact (count (:snake2 @game-state)) => 5)
          (fact (not= (:ball @game-state) [162 108]) => true)
          (fact (:score @game-state) => [1 1])
          (main/update-game-on-eat game-state)
          (fact (count (:snake1 @game-state)) => 5)
          (fact (count (:snake2 @game-state)) => 5)
          (fact (:score @game-state) => [1 1])
          (single/update-game-on-eat game-state)
          (fact (count (:snake1 @game-state)) => 5)
          (swap! game-state (fn [game-state] (assoc game-state :ball [162 108])))
          (single/update-game-on-eat game-state)
          (fact (count (:snake1 @game-state)) => 6)))

 (facts "Testing find-players-by-socket"
        (let [socket1 "sock-1"
              socket2 "sock-2"
              socket3 "sock-3"
              players1 (atom {1 {:socket socket1}
                              2 {:socket socket2}})
              players2 (atom {3 {:socket "other-sock"}})
              online-games {:game-a players1
                            :game-b players2}]

          (fact "finds players atom when socket is present"
                (find-players-by-socket socket1 online-games) => players1)

          (fact "returns nil when socket not present"
                (find-players-by-socket socket3 online-games) => nil)))

 (facts "Testing update-player-direction"
        (let [socket1 "sock-1"
              socket2 "sock-2"
              players (atom {:snake1 {:socket socket1
                                      :direction :up
                                      :change-dir true}
                             :snake2 {:socket socket2
                                      :direction :left
                                      :change-dir true}})
              online-games (atom {:game-1 players})]

          (update-player-direction socket1 :left online-games)
          (fact "snake1 updates from :up to :left"
                (get-in @players [:snake1 :direction]) => :left)

          (update-player-direction socket1 :right online-games)
          (fact "snake1 direction does not change to opposite"
                (get-in @players [:snake1 :direction]) => :left)

          (swap! players assoc-in [:snake2 :change-dir] false)
          (update-player-direction socket2 :up online-games)
          (fact "snake2 does not update when :change-dir is false"
                (get-in @players [:snake2 :direction]) => :left)))

 (facts "Testing generate-random-part"
        (let [game-state {:snake1 [[20 20] [40 20]]
                          :snake2 [[60 20] [80 20]]
                          :parts  [[100 100]]
                          :cake1  {:parts [{:part-id 1 :part-image "img1.png" :amount 2 :current 1}
                                           {:part-id 2 :part-image "img2.png" :amount 1 :current 1}]}
                          :cake2  {:parts [{:part-id 3 :part-image "img3.png" :amount 3 :current 2}
                                           {:part-id 4 :part-image "img4.png" :amount 2 :current 2}]}}
              result (cake/generate-random-part game-state)]

          (fact "result has image, coordinate and part-id"
                (set (keys result)) => #{:image :coordinate :part-id})
          (fact "coordinate is not in blocked-fields"
                (not (contains? (set (:parts game-state)) (:coordinate result))) => true)
          (fact "part-id belongs to cake1 or cake2 available parts"
                (contains? #{1 2 3 4} (:part-id result)) => true)))

 (facts "Testing get-eaten-part"
        (let [parts [{:part-id 1 :coordinate [100 100] :image "img1.png"}
                     {:part-id 2 :coordinate [120 100] :image "img2.png"}]]

          (fact "returns part when head is on a part"
                (cake/get-eaten-part [100 100] parts) => {:part-id 1 :coordinate [100 100] :image "img1.png"})

          (fact "returns nil when head is not on any part"
                (cake/get-eaten-part [140 100] parts) => nil)))

 (facts "Testing update-part-current"
        (let [parts [{:part-id 1 :current 0 :amount 2}
                     {:part-id 2 :current 1 :amount 3}
                     {:part-id 3 :current 2 :amount 2}]]
          (fact "increments :current of the matching part"
                (cake/update-part-current parts 2)
                => [{:part-id 1 :current 0 :amount 2}
                    {:part-id 2 :current 2 :amount 3}
                    {:part-id 3 :current 2 :amount 2}])
          (fact "does nothing if part-id not found"
                (cake/update-part-current parts 99) => parts)
          (fact "only first matching part is updated if duplicates exist"
                (cake/update-part-current [{:part-id 1 :current 0} {:part-id 1 :current 5}] 1)
                => [{:part-id 1 :current 1}
                    {:part-id 1 :current 5}])))

 (facts "Testing update-power-consumed"
        (let [game-state {:snake1 [[0 0] [10 0] [20 0] [30 0] [40 0] [50 0]]
                          :snake2 [[100 0] [110 0] [120 0]]
                          :power "active"}]
          (fact "+3 power adds 3 blocks to opponent"
                (let [result (main/update-power-consumed game-state :snake1 :snake2 "+3")]
                  (count (:snake2 result)) => (+ 3 (count (:snake2 game-state)))
                  (:power result) => nil))
          (fact "-3 power removes 3 segments if snake size > 5"
                (let [result (main/update-power-consumed game-state :snake1 :snake2 "-3")]
                  (count (:snake1 result)) => (- (count (:snake1 game-state)) 3)
                  (:power result) => nil))
          (fact "-3 power does nothing if snake size <= 5"
                (let [small-state (assoc game-state :snake1 [[0 0] [10 0] [20 0] [30 0] [40 0]])
                      result (main/update-power-consumed small-state :snake1 :snake2 "-3")]
                  result => small-state))
          (fact "boom sets :lost to consuming snake"
                (let [result (main/update-power-consumed game-state :snake1 :snake2 "boom")]
                  (:lost result) => :snake1))))