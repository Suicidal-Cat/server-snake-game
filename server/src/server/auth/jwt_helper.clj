(ns server.auth.jwt-helper
  (:require [buddy.sign.jwt :as jwt]
            [aero.core :refer [read-config]]))

(def config (read-config "config.edn"))
(def secret (:jwt-secret config))

(defn generate-token [user-id]
  (jwt/sign {:user-id user-id} secret))

(defn verify-token [token]
  (try
    (jwt/unsign token secret)
    (catch Exception _ nil)))
