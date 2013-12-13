(ns infolog.core
  (:refer-clojure :exclude [==])
  (:require [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]])
  (:import infolog.PrologLexer java.io.ByteArrayInputStream)
  (:use clojure.core.logic
        clojure.core.logic.pldb))

(def prob-home (System/getenv "PROB_HOME"))
(def entry-point "/src/prob_tcltk.pl")
(def temp-file "prob-data_full.clj")
(def db-file "database.clj")

(def facts (atom {}))

(db-rel git sha)
(db-rel dependency importing-module imported-module)
(db-rel module module-name file)
(db-rel problem description)
(db-rel clause module-name predicate-name arity start end)
(db-rel operator module-name predicate-name arity priority fix associativity)
(db-rel call caller-module caller-predicate caller-arity callee-module callee-predicate callee-arity start end)
(db-rel predicate module predicate-name arity)
(db-rel is_dynamic module predicate-name arity)
(db-rel is_volatile module predicate-name arity)
(db-rel is_exported module predicate-name arity)
(db-rel is_multifile module predicate-name arity)
(db-rel is_meta module predicate-name arity args)
(db-rel declared_mode module predicate-name arity args)
(db-rel is_blocking module predicate-name arity args)

(defn transform-comments [module token]
  (let [text (.getText token)
        start-line (-> token .getStartPos .getLine)
        start-column (-> token .getStartPos .getPos)
        end-line (-> token .getEndPos .getLine)
        end-column (-> token .getEndPos .getPos)]
    [module text start-line start-column end-line end-column]))

(defn extract-comments [{:keys [m file]}]
  (when-not (.startsWith file "sicstus/")
    (let [comments (PrologLexer/lexFile file)]
      (map (partial transform-comments m) comments))))

(defn sicstus-call [output-file]
  ["sicstus" "-l" "./prolog-analyzer/analyzer.pl"
   "--goal"
   (str "analyze('" prob-home entry-point "','" output-file "').")])

(defn run-prolog-analyzer [filename]
  (apply sh (sicstus-call filename))
  filename)

(defn get-comments [modules]
  (mapcat extract-comments modules))


(defn- load-pl-output [f] (edn/read-string (slurp f)))
(defn- eval-all [c] (doall (map eval c)))
(defn- mk-db [facts] (apply db facts))

(defn make-db [input-file output-file]
  (let [facts (->> input-file load-pl-output eval-all mk-db)
        mods (with-db facts (run* [q] (fresh [x] (module q x))))
        comments (get-comments mods)]
    (spit output-file facts)
    (spit "comments comments")))

(defn load-database [file]
  (-> file
      slurp
      edn/read-string))

(defn refresh-database []
  (run-prolog-analyzer temp-file)
  (make-db temp-file db-file)
  (load-database db-file))

(defn set-db [mode]
  (let [database (cond
                  (= mode :rebuild) (refresh-database)
                  (= mode :load) (load-database db-file))]
    (reset! facts database)))
