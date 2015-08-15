(defproject infolog "0.1.0-SNAPSHOT"
  :description "ProB Sourcecode Analysis Tool"
  :url "https://github.com/bendisposto/infolog"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.logic "0.8.10"]
                 [de.hhu.stups/infolog-parser "0.1.0"]
                 [org.clojure/data.zip "0.1.1"]
                 [compojure "1.4.0"]
                 [hiccup "1.0.5"]
                 [org.clojure/data.json "0.2.6"]
                 [ring/ring-defaults "0.1.5"]]
  :plugins [[lein-ring "0.9.6"]]
  :ring {:handler infolog.core/app
         :nrepl {:start? tr}}

  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}}
  :main infolog.core
  )
