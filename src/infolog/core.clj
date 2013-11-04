(ns infolog.core
  (:require [infolog.datomic :as db]
            [infolog.prolog :as prolog]
            [datomic.api :as d]))


(println "Loading core ...")

(defprotocol Show
  (show [this] "Creates a human readable representation of an entity"))

(extend-type Object
  Show
  (show [x] (str x)))


(defrecord Module [id name]
  Show
  (show [x] (:name x)))

(defrecord Predicate [id module name arity]
  Show
  (show [x] (str (-> x :module :name) ":" (:name x) "/" (:arity x))))

(def uri "datomic:free://localhost:4334/infolog")



(defn go-mem []
  (let [uri "datomic:free://localhost:4334/infolog"
        _ (prolog/run-prolog-analyzer)
        prolog-data (read-string (slurp prolog/temp-file))
        comments (prolog/get-comments (:modules prolog-data))] 
    (db/populate-db! uri prolog-data comments)))


(comment
  "Some useful queries"

  (d/q '[:find ?m ?p ?a ?pc ?ac :where [?e :prolog/problem true] [?ce :predicate/call ?e]  [?ce :predicate/name ?p] [?ce :predicate/arity ?a] [?e :predicate/name ?pc]  [?e :predicate/arity ?ac] [?ce :predicate/module ?me] [?me :module/file ?m]  ] db)

  (d/q '[:find ?m :where [?m :module/name "haskell_csp_analyzer"]] db)


  (defn module-for-predicate-name [n] (d/q '[:find ?m :in $ ?n :where [?me :module/name ?m] [?p :predicate/module ?me] [?p :predicate/name ?n]] db n))
  
  

  )

