(ns infolog.core
  (:refer-clojure :exclude [==])
  (:require [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [clojure.core.logic.fd :as fd])
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
(db-rel clause module-name predicate-name arity ^:index start end)
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
(db-rel documentation module predicate-name arity text)

(defn predicates-after-line [db m l]
  (with-db db
    (run* [q]
          (fresh [p a s e d]
                 (clause m p a s e)
                 (fd/< l s)
                 (== q [p a s])))))

(defn comment->predicate [db module line]
  (->> (predicates-after-line db module line)
       (sort-by last)
       first
       butlast))

(defn transform-comments [module token]
  (let [text (.getText token)
        start-line (-> token .getStartPos .getLine)
        start-column (-> token .getStartPos .getPos)
        end-line (-> token .getEndPos .getLine)
        end-column (-> token .getEndPos .getPos)]
    [module text start-line end-line]))

(defn assert-comment [db [m text start-line end-line]]
  (let [[p a] (comment->predicate db m end-line)]
    (-> db (db-fact documentation m p a text))))

(defn extract-comments [[m file]]
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
        mods (with-db facts (run* [q] (fresh [m f] (module m f) (== q [m f]))))
        comments (get-comments mods)]
    (spit output-file facts)
    (spit "comments.clj" (pr-str  comments))))

(defn load-database [file]
  (-> file
      slurp
      edn/read-string))

(defn noto [g]
  (fn [a]
    (if (nil? (g a))
      a)))

(comment

  ;; extracting the information from Prolog
  ;; make sure that the PROB_HOME env variable is set
  (run-prolog-analyzer "raw-data.clj")

  ;; Create a database from the extracted information
  (make-db "raw-data.clj" "database.clj")

  ;; load the database for processing
   (def f (load-database "database.clj"))  

  ;; find modules that are loaded but not using use_module
  (with-db f (run* [q] (fresh [m f d] (module m f) (noto (dependency d m)) (== q m))))

  ;; print all problems
  (doseq [x (with-db f (run* [q] (problem q)))] (println x))

  )
