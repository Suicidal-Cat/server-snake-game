(ns server.auth.jwt-middleware
  (:require [ring.util.response :refer [response status]]
            [server.auth.jwt-helper :as jwt-helper]))

(defn wrap-jwt-auth [handler]
  (fn [request]
    (let [auth-header (get-in request [:headers "authorization"])
          token (when auth-header
                  (second (re-find #"(?i)^Bearer (.+)$" auth-header)))]
      (if token
        (if-let [user-data (jwt-helper/verify-token token)]
          (handler (assoc request :identity user-data))
          (-> (response (pr-str {:error "Invalid token"}))
              (status 401)))
        (-> (response (pr-str {:error "Authorization token required"}))
            (status 401))))))