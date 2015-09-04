(ns infolog.handlers
  (:require [re-frame.core :as re-frame]
            [infolog.db :as db]
            [ajax.core :as ajax]
            [cljs.reader :as reader]
            [taoensso.encore :as enc  :refer (logf log logp)]))

(re-frame/register-handler
 :initialize-db
 (fn  [_ _]
   (re-frame/dispatch [:request-problems-csv])
   db/default-db))

(defn common-prefix [sep paths]
  (let [parts-per-path (map #(clojure.string/split % (re-pattern sep)) paths)
        parts-per-position (apply map vector parts-per-path)]
    (clojure.string/join sep
                         (for [parts parts-per-position :while (apply = parts)]
                           (first parts)))))

(defn file-prefix [data]
  (let [files (remove #{"unknown"} (map :file data))]
    (common-prefix "/" files)))

(defn problem->map
  [[category problem-type message module
    predicate file start end hash]]
  {:category category
   :problem-type problem-type
   :message message
   :module module
   :predicate predicate
   :file file
   :start start
   :end end
   :hash hash})

(defn remove-file-prefix [prefix]
  (fn [{:keys [file] :as entry}]
    (assoc entry
           :file
           (clojure.string/replace file prefix "."))))

(defn transform-problems [problems]
  (let [pm (map problem->map problems)
        prefix (file-prefix pm)
        pm' (map (remove-file-prefix (js/RegExp. prefix)) pm)]
    [prefix pm']))



(re-frame/register-handler
 :process-infolog-problems
 (fn [db [_ result]]
   (let [[prefix problems] (transform-problems (:infolog_problem_flat result))
         ]
     (assoc db
            :infolog-problems problems
            :directory prefix
            :modules (into {} (:defined_module result))
            :dependencies (into {} (map (fn [[k v]] [k (map second v)]) (group-by first (:depends_on result))))))))

(re-frame/register-handler
 :bad-response
 (fn [db [_ result]]
   (logp :error result)
   db))

(re-frame/register-handler
 :histo-by-module-switch
 re-frame/debug
 (fn [db [_ problem-type on?]]
   (let [x (get-in db [:histo-by-module :show] #{})
         db' (if on?
               (assoc-in db [:histo-by-module :show] (conj x problem-type))
               (assoc-in db [:histo-by-module :show] (disj x problem-type)))]
     (logp (:histo-by-module db'))
     db')))

(re-frame/register-handler
 :request-problems-csv
 (fn [db _]
   (ajax/GET
    "infolog.edn"
    {:handler       #(re-frame/dispatch [:process-infolog-problems %1])
     :error-handler #(re-frame/dispatch [:bad-response %1])})
   (assoc db :loading-problems-csv? true)))

(re-frame/register-handler
 :set-active-panel
 (fn [db [_ active-panel]]
   (assoc db :active-panel active-panel)))
