(ns server.routes
  (:require
   [clojure.edn :as edn]
   [compojure.core :refer [defroutes OPTIONS POST]]
   [ring.util.response :refer [header response]]
   [server.auth.jwt-helper :as jwt-helper]
   [server.auth.jwt-middleware :refer [wrap-jwt-auth]]
   [server.db.dbBroker :refer [get-available-friend-requests get-leaderboard
                               get-match-history get-pending-friend-requests
                               login register send-friend-request
                               update-friend-request]]))


(defn response-data [value]
  (-> (response (pr-str {:status (if value "successful" "unsuccessful") :value value}))
      (header "Content-Type" "application/edn")))

(defroutes public-routes
  (POST "/login" req
    (let [body (-> req :body slurp edn/read-string)
          email (:email body)
          password (:password body)
          user (login email password)
          token (if user (jwt-helper/generate-token (:id user)) nil)]
      (response-data (if (and user token) {:user user :token token} "Wrong credentials"))))

  (POST "/register" req
    (let [body (-> req :body slurp edn/read-string)
          email (:email body)
          username (:username body)
          password (:password body)
          result (register email username password)]
      (response-data (if result true false))))

  (OPTIONS "*" [] {:status 200}))

(defroutes protected-routes
  (POST "/leaderboard" req
    (let [body (-> req :body slurp edn/read-string)
          userId (:userId body)
          isFriends (:friends body)
          result (get-leaderboard userId isFriends)] 
      (response-data (if result result false))))

  (POST "/match-history" req
    (let [body (-> req :body slurp edn/read-string)
          userId (:userId body)
          result (get-match-history userId)]
      (response-data (if result result false))))

  (POST "/friendlist/send" req
    (let [body (-> req :body slurp edn/read-string)
          senderId (:senderId body)
          receiverId (:receiverId body)
          result (send-friend-request senderId receiverId)]
      (response-data (if result true false))))

  (POST "/friendlist/update-status" req
    (let [body (-> req :body slurp edn/read-string)
          userId1 (:userId1 body)
          userId2 (:userId2 body)
          accepted (:accepted body)
          result (update-friend-request userId1 userId2 accepted)]
      (response-data (if result true false))))

  (POST "/friendlist/available-friend-requests" req
    (let [body (-> req :body slurp edn/read-string)
          userId (:userId body)
          username (:username body)
          result (get-available-friend-requests userId username)]
      (response-data (if result result false))))

  (POST "/friendlist/pending-friend-requests" req
    (let [body (-> req :body slurp edn/read-string)
          userId (:userId body)
          result (get-pending-friend-requests userId)]
      (response-data (if result result false)))))

(def wrapped-protected-routes
  (wrap-jwt-auth protected-routes))