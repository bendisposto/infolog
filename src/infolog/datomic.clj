(ns infolog.datomic
    (:require [datomic.api :as d]))

(def conn (atom nil))
(def progresscount (atom 0))

(def schema
  [
   {:db/id #db/id[:db.part/db]
    :db/ident :git/commit
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Git revision"
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :module/name
    :db/fulltext true
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Module name"
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :module/file
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "File containing the module"
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :module/dependson
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "Module dependency"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :prolog/problem
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/many
    :db/doc "indicates that this definition was added by database import"
    :db.install/_attribute :db.part/db}
   

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/module
    :db/index true
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Module containing predicate"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/name
    :db/fulltext true
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Predictate name"
    :db.install/_attribute :db.part/db}



   
   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/arity
    :db/index true
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "Predictate arity"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/dynamic
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "true iff the predicate is dynamic"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/volatile
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "true iff the predicate is volatile"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/multifile
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "true if the predicate is defined across multiple files"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/exported
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "true iff the predicate is exported"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/mode
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "mode declaration"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/meta
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "meta declaration"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/block
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "blocking declaration"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :operator/fix
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "prefix, postfix, infix"
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :operator/priority
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "operator priority"
    :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db]
    :db/ident :operator/accociativity
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "accociativity of an infix operator: left, right, not"
    :db.install/_attribute :db.part/db }

   {:db/id #db/id[:db.part/db]
    :db/ident :clause/predicate
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "reference to the predicate containing a clause"
    :db.install/_attribute :db.part/db }

   {:db/id #db/id[:db.part/db]
    :db/ident :clause/start
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "line number where the clause starts"
    :db.install/_attribute :db.part/db }
   
   {:db/id #db/id[:db.part/db]
    :db/ident :clause/end
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "line number where the clause ends"
    :db.install/_attribute :db.part/db }

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/call
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "reference to called predicate"
    :db.install/_attribute :db.part/db }

   {:db/id #db/id[:db.part/db]
    :db/ident :predicate/doc
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "documentation"
    :db.install/_attribute :db.part/db }
   
   ])


(defn ensure-schema [conn]
  (or (-> conn d/db (d/entid :module/name))
      @(d/transact conn schema)))

(def tempid? map?)

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


(defn find-module
  "Retrieves the module id from the database. If it does not yet exists, the module will be created and marked as a problem."
  [conn m]
  (let [module (or (ffirst (d/q '[:find ?e :in $ ?name :where [?e :module/name ?name]] (d/db conn) m))
                   (d/tempid :db.part/user))]
    (if (tempid? module)
      (do (println "Missing module: " m)
          @(d/transact conn
                         [[:db/add m :module/name module]
                          [:db/add m :module/file (str "unknown/" module ".pl")]
                          [:db/add m :prolog/problem true]])
          (find-module conn m))
      module)))

(defn mk-predicate-tx
  ([conn {:keys [m p a]}]
     (let [module (find-module conn m)]
       (mk-predicate-tx module p a false)))
  ([module p a] (mk-predicate-tx module p a false))
  ([module p a problem?]
     (let [id (d/tempid :db.part/user)
           default [[:db/add id :predicate/module module]
                    [:db/add id :predicate/name p]
                    [:db/add id :predicate/arity a]]]
       (if problem? (conj default [:db/add id :prolog/problem true])))))

(defn find-predicate [conn m p a]
  (let [module (find-module conn m)
        predicate (or  (ffirst (d/q '[:find ?e :in $ ?module ?name ?arity :where
                                      [?e :predicate/name ?name]
                                      [?e :predicate/arity ?arity] 
                                      [?e :predicate/module ?module]]
                                    (d/db conn) module p a))
                       (d/tempid :db.part/user))]
    (if (tempid? predicate)
      (do
        (println "Missing predicate " m ":" p "/" a)
        @(d/transact conn (mk-predicate-tx module p a true))
        (find-predicate conn m p a))
      predicate)))



(defn mk-operator-tx [conn {:keys [m p a prio fix associativity]}]
  (let [id (find-predicate conn m p a)
        base [ [:db/add id :operator/fix fix]
               [:db/add id :operator/priority prio]]]
    (if (= "infix" fix) (conj base [:db/add id :operator/accociativity associativity]) base)))

(defn mk-mode-tx [conn k {:keys [m p a args]}]
  [[:db/add (find-predicate conn m p a)  k args]])

(defn mk-bool-tx [conn k {:keys [m p a]}]
  (let [id (find-predicate conn m p a)]
    (println "Setting " k " for " id " - " m ":" p "/" a)
    [[:db/add id k true]]))

(defn mk-call-tx [conn {:keys [m p a cm cp ca]}]
  (let [caller (find-predicate conn m p a)
        callee (find-predicate conn cm cp ca)]
    (swap! progresscount inc)
    (when (== 0 (mod @progresscount 1000)) (println @progresscount))
    [[:db/add caller :predicate/call callee]]))

(defn mk-clause-tx [conn {:keys [m p a start end]}]
  (let [id (d/tempid :db.part/user)
        pid (find-predicate conn m p a)]
    (swap! progresscount inc)
    (when (== 0 (mod @progresscount 1000)) (println @progresscount))
    [[:db/add id :clause/predicate pid]
     [:db/add id :clause/start start]
     [:db/add id :clause/end end]]))

(defn delete-database [uri] (d/delete-database uri))

(defn populate-db! [prolog-data]
  (let [conn @conn
        commit (ffirst (d/q '[:find ?e ?c :where [?e :git/commit ?c]] (d/db conn)))]
    (when-not commit
      @(d/transact conn (let [id (d/tempid :db.part/user)] [[:db/add id :module/name "user"] [:db/add id :module/file "sicstus/user.pl"]]))
      @(d/transact conn (let [id (d/tempid :db.part/user)] [[:db/add id :module/name "built_in"] [:db/add id :module/file "sicstus/built_in.pl"]]))
      @(d/transact conn (let [id (d/tempid :db.part/user)] [[:db/add id :module/name "undefined_module"] [:db/add id :module/file "sicstus/undefined_module.pl"]]))
      @(d/transact conn [[:db/add (d/tempid :db.part/user) :git/commit (:git prolog-data)]])
      @(d/transact conn (mapcat mk-module-tx (:modules prolog-data)))
      @(d/transact conn (mapcat (partial mk-dependency-tx conn) (:dependencies prolog-data)))
      (println "Processing predicates (" (count (:predicates prolog-data)) ")")
      @(d/transact conn (mapcat (partial mk-predicate-tx conn) (:predicates prolog-data)))
      (println "Processing predicate attribute: " "dynamic")
      @(d/transact conn (mapcat (partial mk-bool-tx conn :predicate/dynamic) (:dynamic prolog-data)))
      (println "Processing predicate attribute: " "volatile")
      @(d/transact conn (mapcat (partial mk-bool-tx conn :predicate/volatile) (:volatile prolog-data)))
      (println "Processing predicate attribute: " "multifile")
      @(d/transact conn (mapcat (partial mk-bool-tx  conn :predicate/multifile) (:multifile prolog-data)))
      (println "Processing predicate attribute: " "exported")     
      @(d/transact conn (mapcat (partial mk-bool-tx  conn :predicate/exported) (:exported prolog-data)))
      (println "Processing predicate attribute: " "mode")
      @(d/transact conn (mapcat (partial mk-mode-tx  conn :predicate/mode) (:mode prolog-data)))
      (println "Processing predicate attribute: " "meta")
      @(d/transact conn (mapcat (partial mk-mode-tx  conn :predicate/meta) (:meta prolog-data)))
      (println "Processing predicate attribute: " "block")
      @(d/transact conn (mapcat (partial mk-mode-tx  conn :predicate/block) (:blocking prolog-data)))
      (println "Processing operators")
      @(d/transact conn (mapcat (partial mk-operator-tx  conn) (:operators prolog-data)))
      (println "Processing calls (" (count (:calling prolog-data)) ")")
      @(d/transact conn (mapcat (partial mk-call-tx  conn) (:calling prolog-data)))
      (println "Processing clauses (" (count (:clauses prolog-data)) ")")
      (reset! progresscount 0)
      @(d/transact conn (mapcat (partial mk-clause-tx  conn) (:clauses prolog-data))))))

