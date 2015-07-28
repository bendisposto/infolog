(defproject infolog "0.1.0-SNAPSHOT"
  :description "ProB Sourcecode Analysis Tool"
  :url "https://github.com/bendisposto/infolog"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["cobra" "http://cobra.cs.uni-duesseldorf.de/artifactory/repo"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.logic "0.8.10"]
                 [infolog/doc_extractor "0.0.1-20131008.113111-1"]
                 [org.clojure/data.zip "0.1.1"]]
                 :main infolog.core)
