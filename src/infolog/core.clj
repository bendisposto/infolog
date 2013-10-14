(ns infolog.core
  (:import infolog.PrologLexer)
  (:import java.io.ByteArrayInputStream)
  (:require [clojure.java.shell :refer [sh]]
            [infolog.datomic :as db]))

(defn transform-comments [token]
  (let [text (.getText token)
        start-line (-> token .getStartPos .getLine)
        start-column (-> token .getStartPos .getPos)
        end-line (-> token .getEndPos .getLine)
        end-column (-> token .getEndPos .getPos)]
    {:text text
     :start-line start-line
     :start-column start-column
     :end-line end-line
     :end-column end-column}))


(def prob-home (System/getenv "PROB_HOME"))
(def entry-point "/src/prob_tcltk.pl")
(def temp-file "prob-data.clj")
(def prolog-data (atom nil))
;(def uri "datomic:mem://infolog")
(def uri "datomic:free://localhost:4334/infolog22")


(defn extract-comments [filename]
  (when-not (.startsWith filename "sicstus/") (let [comments (PrologLexer/lexFile filename)]
             [filename (map transform-comments comments)])))

(def sicstus-call
  ["sicstus" "-l" "./prolog-analyzer/analyzer.pl"
   "--goal"
   (str "analyze('" prob-home entry-point "','" temp-file "').")])

(defn run-prolog-analyzer []
  (apply sh sicstus-call))

(defn read-prolog-info! []
  (let [d (read-string (slurp temp-file))]
    (reset! prolog-data d))
  nil)

(defn analyze []
  (let [data @prolog-data
        comments (into {} (filter (comp seq second) (map extract-comments (map :file (:modules data)))))]
    [comments data]))


(defn go [x]
  (let [uri (str "datomic:free://localhost:4334/infolog" x)]
    (read-prolog-info!)
    (db/ensure-db uri)
    (db/populate-db! @prolog-data))
  :done)

(defn go-mem []
  (let [uri "datomic:mem://infolog"]
    (db/delete-database uri)
    (read-prolog-info!)
    (db/ensure-db uri)
    (db/populate-db! @prolog-data))
  :done)


