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

(re-frame/register-handler
 :process-infolog-problems
 re-frame/debug
 (fn [db [_ result]]
   (let [data (rest (js->clj (. js/CSV parse result)))]
     (logp :received (count data) :datasets)
     (assoc db :infolog-problems data))))

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
