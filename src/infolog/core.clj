(ns infolog.core
  (:require [infolog.datomic :as db]
            [infolog.prolog :as prolog]))

(def prolog-data (atom nil))
(def uri "datomic:free://localhost:4334/infolog22")

(defn read-prolog-info! []
  (let [d (read-string (slurp prolog/temp-file))]
    (reset! prolog-data d))
  nil)

;; (defn analyze []
;;   (let [data @prolog-data
;;         comments 
;;     [comments data]))

(defn go [x]
  (let [uri (str "datomic:free://localhost:4334/infolog" x)]
    (read-prolog-info!)
    (db/populate-db! (db/init-db uri) @prolog-data))
  :done)

(defn go-mem []
  (let [uri "datomic:mem://infolog"
        prolog-data (read-string (slurp prolog/temp-file))
        comments (prolog/get-comments (:modules prolog-data))]
    (db/populate-db! uri prolog-data comments)))


