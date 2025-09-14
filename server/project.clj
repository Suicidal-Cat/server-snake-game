(defproject server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [ring/ring-core "1.13.0"]
                 [ring/ring-jetty-adapter "1.13.0"]
                 [ring-cors "0.1.13"]
                 [compojure "1.7.1"]
                 [aero "1.1.6"]
                 [com.github.seancorfield/next.jdbc "1.3.955"]
                 [com.mysql/mysql-connector-j "8.1.0"]
                 [buddy/buddy-hashers "2.0.167"]
                 [buddy/buddy-sign "3.4.333"]]
  :main ^:skip-aot server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[midje "1.10.9"]]
                   :plugins [[lein-midje "3.2.2"]]}})
