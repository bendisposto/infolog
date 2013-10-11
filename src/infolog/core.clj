(ns infolog.core
  (:import infolog.PrologLexer)
  (:import java.io.ByteArrayInputStream)
  (:require [clojure.java.shell :refer [sh]]
            [infolog.datomic :refer [schema]]
            [datomic.api :as d]))

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
(def conn (atom nil))

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

(defn ensure-schema [conn]
  (or (-> conn d/db (d/entid :module/name))
      @(d/transact conn schema)))

(defn ensure-db [db-uri]
  (let [newdb? (d/create-database db-uri)
        c (d/connect db-uri)]
    (ensure-schema c)
    (reset! conn c)))

(defn mk-module-tx [{:keys [m file]}]
  (let [id (d/tempid :db.part/user)]
    [[:db/add id :module/name m]
     [:db/add id :module/file file]]))

(defn mk-dependency-tx [conn {:keys [local_module imported_module]}]
  (let [local (ffirst (d/q '[:find ?e :in $ ?name :where [?e :module/name ?name]] (d/db conn) local_module))
        imported (ffirst (d/q '[:find ?e :in $ ?name :where [?e :module/name ?name]] (d/db conn) imported_module))]
    [[:db/add local :module/dependson imported]]))

(defn mk-predicate-tx [conn {:keys [m p a problem] :as all}]
  (println "foo " all)
  (let [id (d/tempid :db.part/user)
        module (ffirst (d/q '[:find ?e :in $ ?name :where [?e :module/name ?name]] (d/db conn) m))]
    (if module
      [[:db/add id :predicate/module module]
       [:db/add id :predicate/name p]
       [:db/add id :predicate/arity a]]
      (let [m (d/tempid :db.part/user)
            base [[:db/add m :module/name module]
                  [:db/add m :module/file (str "unknown/" module ".pl")]
                  [:db/add m :prolog/problem true]
                  [:db/add id :predicate/module m]
                  [:db/add id :predicate/name p]
                  [:db/add id :predicate/arity a]
                  ]]
        (if problem (conj base [:db/add id :prolog/problem true]) base)))))

(defn find-predicate [conn m p a]
  (println "fp" m p a)
  (or (ffirst (d/q '[:find ?e :in $ ?module ?name ?arity :where
                     [?e :predicate/name ?name]
                     [?e :predicate/arity ?arity] 
                     [?e :predicate/module ?m]
                     [?m :module/name ?module]
                     ] (d/db conn) m p a))
      (do
        (println "bug1")
        @(d/transact conn (mk-predicate-tx conn {:m m :p p :a a :problem true}))
        (ffirst (d/q '[:find ?e :in $ ?module ?name ?arity :where
                       [?e :predicate/name ?name]
                       [?e :predicate/arity ?arity] 
                       [?e :predicate/module ?m]
                       [?m :module/name ?module]
                       ] (d/db conn) m p a)))))

(defn mk-operator-tx [conn {:keys [m p a prio fix associativity]}]
  (let [id (find-predicate conn m p a)
        base [ [:db/add id :operator/fix fix]
               [:db/add id :operator/priority prio]]]
    (if (= "infix" fix) (conj base [:db/add id :operator/accociativity associativity]) base)))

(defn mk-mode-tx [conn k {:keys [m p a args]}]
  [[:db/add (find-predicate conn m p a)  k args]])

(defn mk-bool-tx [conn k {:keys [m p a]}]
  [[:db/add (find-predicate conn m p a)  k true]])

(defn mk-call-tx [conn {:keys [m p a cm cp ca]}]
  (let [caller (find-predicate conn m p a)
        callee (find-predicate conn cm cp ca)]
    [[:db/add caller :predicate/call callee]]))

(defn mk-clause-tx [conn {:keys [m p a start end]}]
  (let [id (d/tempid :db.part/user)
        pid (find-predicate conn m p a)]
    [[:db/add id :clause/predicate pid]
     [:db/add id :clause/start start]
     [:db/add id :clause/end end]]))


(defn populate-db! [conn prolog-data]
  (let [commit (ffirst (d/q '[:find ?e ?c :where [?e :git/commit ?c]] (d/db conn)))]
    (when-not commit
      @(d/transact conn (let [id (d/tempid :db.part/user)] [[:db/add id :module/name "user"] [:db/add id :module/file "sicstus/user.pl"]]))
      @(d/transact conn (let [id (d/tempid :db.part/user)] [[:db/add id :module/name "built_in"] [:db/add id :module/file "sicstus/built_in.pl"]]))
      @(d/transact conn (let [id (d/tempid :db.part/user)] [[:db/add id :module/name "undefined_module"] [:db/add id :module/file "sicstus/undefined_module.pl"]]))
      @(d/transact conn [[:db/add (d/tempid :db.part/user) :git/commit (:git prolog-data)]])
      @(d/transact conn (mapcat mk-module-tx (:modules prolog-data)))
      @(d/transact conn (mapcat (partial mk-dependency-tx conn) (:dependencies prolog-data)))
      @(d/transact conn (mapcat (partial mk-predicate-tx conn) (:predicates prolog-data)))
      @(d/transact conn (mapcat (partial mk-bool-tx conn :predicate/dynamic) (:dynamic prolog-data)))
      @(d/transact conn (mapcat (partial mk-bool-tx conn :predicate/volatile) (:volatile prolog-data)))
      @(d/transact conn (mapcat (partial mk-bool-tx  conn :predicate/multifile) (:multifile prolog-data)))
      @(d/transact conn (mapcat (partial mk-bool-tx  conn :predicate/exported) (:exported prolog-data)))
      @(d/transact conn (mapcat (partial mk-mode-tx  conn :predicate/mode) (:mode prolog-data)))
      @(d/transact conn (mapcat (partial mk-mode-tx  conn :predicate/meta) (:meta prolog-data)))
      @(d/transact conn (mapcat (partial mk-mode-tx  conn :predicate/block) (:blocking prolog-data)))
      @(d/transact conn (mapcat (partial mk-operator-tx  conn) (:operators prolog-data)))
      @(d/transact conn (mapcat (partial mk-call-tx  conn) (:calling prolog-data)))
      @(d/transact conn (mapcat (partial mk-clause-tx  conn) (:clauses prolog-data))))))

(defn go [x]
  (let [uri (str "datomic:free://localhost:4334/infolog" x)]
    (read-prolog-info!)
    (ensure-db uri)
    (populate-db! @conn @prolog-data))
  :done)

