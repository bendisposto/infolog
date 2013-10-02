(defproject infolog "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["cobra" "http://cobra.cs.uni-duesseldorf.de/artifactory/repo"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.datomic/datomic-free "0.8.4159"]
                 [infolog/doc_extractor "0.0.1-20130924.071030-1"]
                 [org.clojure/data.zip "0.1.1"]]
                 :main infolog.core)


