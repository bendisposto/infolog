(ns infolog.core
  (:refer-clojure :exclude [==])
  (:require [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [clojure.core.logic.fd :as fd]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [hiccup.middleware :refer (wrap-base-url)]
            [hiccup.core :as h]
            [clojure.data.json :as json])
  (:import infolog.PrologLexer java.io.ByteArrayInputStream)
  (:use clojure.core.logic
        clojure.core.logic.pldb))

(def entry-point "/src/prob_tcltk.pl")
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

(defn sicstus-call [prob-home output-file]
  ["sicstus" "-l" "./prolog-analyzer/analyzer.pl"
   "--goal"
   (str "analyze('" prob-home entry-point "','" output-file "').")])

(defn run-prolog-analyzer [prob-home filename]
  (apply sh (sicstus-call prob-home filename))
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

(def sicstus-module #{"lists" "avl" "file_systems" "codesio" "ordsets" "timeout"
                      "chr" "clpfd" "system" "between" "samsort" "random" "atts"
                      "terms" "sets" "gauge" "trees" "assoc" "xml" "process"
                      "aggregate" "tcltk" "heaps" })

(defn modules [f]
  (with-db f (run* [q] (fresh [f] (module q f)))))

(defn dependencies [f]
  (with-db f (run* [q] (fresh [m d] (dependency d m) (== q [d m])))))


(defn dependency-graph [f]
  (str "digraph dependencies {\n"
       (clojure.string/join "," (map (fn [e] (str \" e \")) (take 5 (modules f))))
       (clojure.string/join
        "\n"
        (map (fn [[d m]] (str \" d \" "->" \" m \")) (take 500 (dependencies f)))) "}"))

(defn fix-module [m]
  (clojure.string/join "_" (clojure.string/split m #"/")))

(defn mk-dep [f]
  (let [m-dep (into {} (remove sicstus-module (map (fn [m] [(fix-module m) []]) (modules f))))
        g-dep (group-by first (dependencies f))
        t-dep (remove nil? (map (fn [[k v]] (when (not (sicstus-module k)) [(fix-module k) (remove nil? (mapv (fn [[_ m]] (when (not (sicstus-module m)) (fix-module m))) v))])) (merge m-dep g-dep)))]
    (map (fn [[k v]] {:name k :size (* 1000 (count v)) :imports v}) t-dep)))

(defn details [f m]
  (let [imports (with-db f (run* [q] (fresh [d] (dependency  m d) (== q d))))
        imported-by (with-db f (run* [q] (fresh [d] (dependency  d m) (== q d))))]
    {:name m :imports imports :imported_by imported-by}))

(defn patho [n m]
  (fresh [x] (dependency n x) (patho x m)))

(defn cyclic-dependencies [f]
  (with-db f (run* [q] (fresh [m f] (module m f) (patho m m) (== q m)))))

(defroutes app-routes
  (GET "/" [] (h/html [:h1 "Infolog"] [:a {:href "dependencies.html"} "Module Dependency Visualization"]))
  (GET "/dependencies" [] (json/write-str (mk-dep @facts)))
  (GET "/cycles" [] (json/write-str (cyclic-dependencies @facts)))
  (GET "/dependency-details/:module" [module] (json/write-str (details @facts module)))
  (route/resources "/"))

(def app (do (reset! facts (load-database "database.clj"))
             (-> (handler/site app-routes)
                 (wrap-base-url))))


(comment

  ;; extracting the information from Prolog
  ;; make sure that the PROB_HOME env variable is set
  (run-prolog-analyzer (System/getenv "PROB_HOME") "raw-data.clj")

  ;; Create a database from the extracted information
  (make-db "raw-data.clj" "database.clj")

  ;; load the database for processing
  (def f (load-database "database.clj"))

  ;; find modules that are loaded but not using use_module
  (with-db f (run* [q] (fresh [m f d] (module m f) (noto (dependency d m)) (== q m))))

  ;; print all problems
  (doseq [x (with-db f (run* [q] (problem q)))] (println x))


  )
