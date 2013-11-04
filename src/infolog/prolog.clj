(ns infolog.prolog
  (:import infolog.PrologLexer)
  (:import java.io.ByteArrayInputStream)
  (:require [clojure.java.shell :refer [sh]]))

(def prob-home (System/getenv "PROB_HOME"))
(def entry-point "/src/prob_tcltk.pl")
(def temp-file "prob-data_full.clj")


(defn print-statistics [prolog-data comments]
  (println (count (:modules prolog-data)) "Modules")
  (println (count (:dependencies prolog-data)) "dependencies between modules")
  (println (count (:clauses prolog-data)) "clauses in")
  (println (count (:predicates prolog-data)) "predicates of which")
  (println " - " (count (:exported prolog-data)) "are exported")
  (println " - " (count (:dynamic prolog-data)) "are dynamic predicates")
  (println " - " (count (:blocking prolog-data)) "have blocking declarations")
  (println " - " (count (:multifile prolog-data)) "are declared to be multifile predicates")
  (println " - " (count (:mode prolog-data)) "have mode declarations")
  (println " - " (count (:meta prolog-data)) "are meta predicates")
  (println " - " (count (:volatile prolog-data)) "are volatile predicates")
  (println (count (:operators prolog-data)) "operators")
  (println (count (:calling prolog-data)) "calls")
  (println (count comments) "Prolog-Docs")
  (println)
  (let [percent (* 100.0 (/ (count comments) (count (:predicates prolog-data))))]
    (println (str (format "%.3f" percent) "% of the predicates have documentation")))
  (println)
  (println "The analyzer found" (count (into #{}  (:problems prolog-data))) "distinct problems, the first five are:")
  (doseq [e (take 5 (into #{} (:problems prolog-data)))] (println " -" e)))


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

