(ns infolog.prolog
  (:import infolog.PrologLexer)
  (:import java.io.ByteArrayInputStream)
  (:require [clojure.java.shell :refer [sh]]))

(def prob-home (System/getenv "PROB_HOME"))
(def entry-point "/src/prob_tcltk.pl")
(def temp-file "prob-data_full.clj")


(defn transform-comments [module token]
  (let [text (.getText token)
        start-line (-> token .getStartPos .getLine)
        start-column (-> token .getStartPos .getPos)
        end-line (-> token .getEndPos .getLine)
        end-column (-> token .getEndPos .getPos)]
    {:module module
     :text text
     :start-line start-line
     :start-column start-column
     :end-line end-line
     :end-column end-column}))

(defn extract-comments [{:keys [m file]}]
  (when-not (.startsWith file "sicstus/")
    (let [comments (PrologLexer/lexFile file)]
      (map (partial transform-comments m) comments))))


(def sicstus-call
  ["sicstus" "-l" "./prolog-analyzer/analyzer.pl"
   "--goal"
   (str "analyze('" prob-home entry-point "','" temp-file "').")])

(defn run-prolog-analyzer []
  (apply sh sicstus-call))

(defn get-comments [modules]
  (mapcat extract-comments modules))

