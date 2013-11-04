(ns infolog.datomic
  (:require [datomic.api :as d])
  (:use infolog.schema))


(def conn (atom nil))

(def schema-query '[:find ?v :where [?v :db/ident :infolog/schema-version]])

(def tempid? map?)

(defn init-db [db-uri]
  (let [_ (d/create-database db-uri)
        c (d/connect db-uri)
        db (d/db c)
        schema-loaded (ffirst (d/q schema-query db))
        _ (if schema-loaded db 
             (:db-after (do @(d/transact c schema)
                            @(d/transact c schema-version))))
        ]
        (reset! conn c)
    c))



(defn mkid [] (d/tempid :db.part/user))

(defn get-module [db name]
  (let [id  (or (ffirst (d/q '[:find ?id :in $ ?m :where [?id :module/name ?m]] db name))
                (mkid))] id))

(defn get-predicate [db m p a]
;  (println "p" m p a)
  (let [id  (or (ffirst (d/q '[:find ?id :in $ ?m ?p ?a :where
                               [?id :predicate/name ?p]
                               [?id :predicate/module ?mid]
                               [?id :predicate/arity ?a]
                               [?mid :module/name ?m]] db m p a))
                (mkid))] id))

(defn dbf-module [db {:keys [m file]}])


(defn tx-problem [& ids]
  (into [] (for [id ids] [:db/add id :prolog/problem true])))

(defn tx-module
  ([db m] (tx-module db (mkid) m))
  ([db id {:keys [m file]}]
     [[:db/add id :module/file file]
      [:db/add id :module/name m]]))

(defn tx-predicate
  ([db m] (tx-predicate db (mkid) m))
  ([db id {:keys [m p a]}]
     (let [m-id (get-module db m)]
       (concat [[:db/add id :predicate/module m-id]
                [:db/add id :predicate/name p]
                [:db/add id :predicate/arity a]]
               (when (tempid? m-id)
                 (concat (tx-module db m-id {:m m :file (str "unknown/" m ".pl")})
                         (tx-problem id m-id)))))))


(defn tx-clause [db {:keys [m p a start end]}]
  (let [id (mkid)
        p-id (get-predicate db m p a)]
    (concat [[:db/add id :clause/predicate p-id]
             [:db/add id :clause/start start]
             [:db/add id :clause/end end]]
            (when (tempid? p-id)
              (concat (tx-predicate db p-id { :m m :p  p :a a})
                      (tx-problem db p-id id))))))


(defn tx-call [db {:keys [m p a cm cp ca]}]
  (let [caller (get-predicate db m p a)
        callee (get-predicate db cm cp ca)
        tx-data [[:db/add caller :predicate/call callee]]
        tx-data (concat tx-data
                        (when (tempid? caller)
                          (tx-predicate db caller {:m m :p p :a a})))
        tx-data (concat tx-data
                        (when (tempid? callee)
                          (tx-predicate db callee {:m m :p p :a a})))] tx-data))



(defn tx-dependency [db {:keys [local_module imported_module] :as dep}]
  (let [local-id (get-module db local_module)
        imported-id (get-module db imported_module)
        dep-tx [[:db/add local-id :module/dependson imported-id]]
        dep-tx (concat dep-tx (when (tempid? local-id)
                               [[:db/add local-id :module/name local_module]
                                [:db/add local-id :module/file (str "unknown/" local_module ".pl")]
                                [:db/add local-id :prolog/problem true]]))
        dep-tx (concat dep-tx (when (tempid? imported-id)
                               [[:db/add imported-id :module/name imported_module]
                                [:db/add imported-id :module/file (str "unknown/" imported_module ".pl")]
                                [:db/add imported-id :prolog/problem true]]))]  dep-tx))



(defn tx-arg [prop db {:keys [m p a args]}]
  (let [id (get-predicate db m p a)
        kw (keyword (str "predicate/" prop))
        tx-data [[:db/add id kw args]]
        tx-data (concat tx-data
                        (when (tempid? id)
                          (tx-predicate db id {:m m :p p :a a})))] tx-data))

(defn tx-bool [prop db m] (tx-arg prop db (assoc m :args true)))

(defn tx-operator [db {:keys [m p a prio fix associativity]}]
  (let [id (get-predicate db m p a)]
    [[:db/add id :operator/priority prio]
     [:db/add id :operator/fix fix]
     [:db/add id :operator/associativity associativity]]))

(defn tx-map [db f collection]
  (doall (mapcat (partial f db) collection)))

(defn tx-comment [db {:keys [module text end-line]}]
  (let [id (ffirst
            (sort-by last
                     (d/q '[:find ?pid ?m ?p ?a ?cstart
                            :in $ ?m ?after-line
                            :where
                            [?mid :module/name ?m]
                            [?clause :clause/predicate ?pid]
                            [?pid :predicate/module ?mid]
                            [?pid :predicate/name ?p]
                            [?clause :clause/start ?cstart]
                            [?pid :predicate/arity ?a]
                            [(< ?after-line ?cstart)]]
                          db module end-line)))]
    [:db/add id :predicate/doc text]))  

;(defn schema-version [db] (ffirst (d/q '[:find ?v :where [_ :infolog/schema-version ?v]] db)))

(defn tx-problem-entry [db e]
  [[:db/add (mkid)
    :analyzer/problem e]])


(defn populate-db! [uri prolog-data comments]
  (println "Deleting db @" uri)
  (d/delete-database uri)
  (let [conn (init-db uri)
        db (d/db conn)
        db (:db-after @(d/transact conn (mapcat (partial tx-problem-entry db) (:problems prolog-data))))
        db (:db-after @(d/transact conn (tx-module db {:m "user" :file "sicstus/user.pl"})))
        db (:db-after @(d/transact conn (tx-module db {:m "built_in" :file "sicstus/built_in.pl"})))
        db (:db-after @(d/transact conn (tx-map db tx-module (:modules prolog-data))))
        db (:db-after @(d/transact conn (tx-map db tx-dependency (:dependencies prolog-data))))
        db (:db-after @(d/transact conn (tx-map db tx-predicate (:predicates prolog-data))))
        db (:db-after @(d/transact conn (tx-map db tx-clause (:clauses prolog-data))))
        db (:db-after @(d/transact conn (tx-map db tx-operator (:operators prolog-data))))
        db (:db-after @(d/transact conn (tx-map db tx-call (:calling prolog-data))))
        db (:db-after @(d/transact conn (tx-map db (partial tx-bool "dynamic") (:dynamic prolog-data))))
        db (:db-after @(d/transact conn (tx-map db (partial tx-bool "volatile") (:volatile prolog-data))))
        db (:db-after @(d/transact conn (tx-map db (partial tx-bool "multifile") (:multifile prolog-data))))
        db (:db-after @(d/transact conn (tx-map db (partial tx-bool "exported") (:exported prolog-data))))
        db (:db-after @(d/transact conn (tx-map db (partial tx-arg "meta") (:meta prolog-data))))
        db (:db-after @(d/transact conn (tx-map db (partial tx-arg "mode") (:mode prolog-data))))
        db (:db-after @(d/transact conn (tx-map db (partial tx-arg "block") (:blocking prolog-data))))
        db (:db-after @(d/transact conn (map (partial tx-comment db) comments)))
       ]
    (println "Problems" (d/q '[:find ?m :where [?e :module/name ?m]
                               [?e :prolog/problem true]] db))
    (println "Modules:" (count  (d/q '[:find ?m :where [?e :module/name ?m]] db)))
    (println "Predicates:" (count  (d/q '[:find ?p :where [?e :predicate/name ?p]] db)))
    (println "Clauses:" (count  (d/q '[:find ?e :where [?e :clause/predicate ?c]] db)))
    db))

