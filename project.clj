(defproject infolog "0.1.0-SNAPSHOT"
  :description "ProB Sourcecode Analysis Tool"
  :url "https://github.com/bendisposto/infolog"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.logic "0.8.10"]
                 [de.hhu.stups/infolog-parser "0.1.0"]
                 [org.clojure/data.zip "0.1.1"]]
                 :main infolog.core)
