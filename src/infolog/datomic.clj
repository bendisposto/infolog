(ns infolog.datomic
    (:require [datomic.api :as d]))

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
