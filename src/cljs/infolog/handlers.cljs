(ns infolog.handlers
  (:require [re-frame.core :as re-frame]
            [infolog.db :as db]
            [ajax.core :as ajax]
            [cljsjs.csv]
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

(re-frame/register-handler
 :process-infolog-problems
 (fn [db [_ result]]
   (let [raw-data (rest (js->clj (. js/CSV parse result)))
         data-map (mapv problem->map raw-data)
         prefix (file-prefix data-map)
         data (map (remove-file-prefix (js/RegExp. prefix)) data-map)]
     (assoc db
            :infolog-problems data
            :directory prefix))))

(re-frame/register-handler
 :bad-response
 (fn [db [_ result]]
   (logp :error result)
   db))

(re-frame/register-handler
 :request-problems-csv
 (fn [db _]
   (ajax/GET
    "infolog_problems.csv"
    {:handler       #(re-frame/dispatch [:process-infolog-problems %1])
     :error-handler #(re-frame/dispatch [:bad-response %1])})
   (assoc db :loading-problems-csv? true)))

(re-frame/register-handler
 :set-active-panel
 (fn [db [_ active-panel]]
   (assoc db :active-panel active-panel)))
