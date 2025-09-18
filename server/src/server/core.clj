(ns server.core
  (:gen-class)
  (:require
   [aero.core :refer [read-config]]
   [clojure.edn :as edn]
   [compojure.core :refer [GET routes]]
   [ring.adapter.jetty :refer [run-jetty]]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.websocket :as ws]
   [server.db.dbBroker :refer [game-mode-enum]]
   [server.game.cake-game :as cake]
   [server.game.main-game :as main]
   [server.game.singleplayer-game :as single]))

(def config (read-config "config.edn"))
(def players-queue (atom []))

;try to find available player and start the game
(defn find-game [player-socket data]
  (let [game-mode (:game-mode data)
        new-player {:id (:id data) :socket player-socket :game-mode game-mode}]
    (if (:single data) (single/start-game-single new-player)
        (if (empty? @players-queue)
          (swap! players-queue conj new-player)
          (let [player (some #(when (and (= game-mode (:game-mode %)) (not= (:id %) (:id new-player))) %) @players-queue)]
            (if player
              (do
                (swap! players-queue (fn [players] (filterv #(not= (:id %) (:id player)) players)))
                (if (= game-mode (:time game-mode-enum))
                  (main/start-main-game player new-player)
                  (cake/start-cake-game player new-player)))
              (swap! players-queue conj new-player)))))))


;process messages form client's web socket
(defn handle-message [socket message]
  (let [data (edn/read-string message)
        dir (:direction data)]
    (if (int? (:id data))
      (find-game socket data)
      (cond
        (:time data) (main/change-direction socket dir)
        (:cake data) (cake/change-direction socket dir)
        (:single data) (single/change-direction-single socket dir)))))

(defn echo-handler [request]
  (assert (ws/upgrade-request? request))
  {::ws/listener
   {:on-open
    (fn [socket])
    :on-message
    (fn [socket message]
      (if (= message "exit")
        (ws/close socket)
        (handle-message socket message)))
    :on-close
    (fn [socket code reason] ())}})

(def all-routes
  (routes
   (GET "/ws" [] echo-handler)
  ;;  public-routes
  ;;  protected-routes))
  ))

(def app
  (-> all-routes
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put :delete])))

(defn -main [& args]
  (let [port (Integer/parseInt
              (or (System/getenv "PORT")
                  (str (:server-port config))
                  "3000"))]
    (println "Starting server on port" port)
    (run-jetty app {:host "0.0.0.0" :port port :join? false})))

