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
        parts-per-position (apply map vector parts-per-path)
        pathparts (for [parts parts-per-position :while (apply = parts)] (first parts))]

    (clojure.string/join sep pathparts)))

(defn file-prefix [data]
  (let [files (->> data
                   (map :file)
                   (remove #{"unknown"})
                   (remove #(goog.string.startsWith % "sicstus")))
        prefix (common-prefix "/" (into [] files))]
    prefix))

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
           (clojure.string/replace-first file prefix "."))))

(defn transform-problems [problems]
  (let [pm (map problem->map problems)
        prefix (file-prefix pm)
        pm' (map (remove-file-prefix (js/RegExp. prefix)) pm)
        ]
    [prefix pm']))


(defn call->map [[cm cp ca m p a sl el]]
  {:caller-module cm
   :caller-predicate cp
   :caller-arity ca
   :callee-module m
   :callee-predicate p
   :callee-arity a
   :start sl
   :end el})

(defn transform-calls [calls]
  (map call->map calls))

(re-frame/register-handler
 :process-infolog-problems
 (fn [db [_ r]]
   (let [;;start (goog.date.DateTime.)
         result (if (map? r) r (cljs.reader/read-string r))
         [prefix problems] (transform-problems (:infolog_problem_flat result))
         calling (transform-calls (:calling result))
         deps (into #{} (map (fn [{:keys [caller-module callee-module]}] [caller-module callee-module]) calling))
         db' (assoc db
                    :infolog-problems problems
                    :directory prefix
                    :modules (into {} (:defined_module result))
                    :dependencies deps)
         ;;end (goog.date.DateTime.)
         ]
     ;;(logp :time-process-input start end)
     db')))

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
     db')))

(re-frame/register-handler
 :switch-dep-module-sort
 (fn [db [_ sorting]]
   (assoc db :dep-sort-modules sorting)))

(re-frame/register-handler
 :request-problems-csv
 (fn [db _]
   (ajax/GET
    "infolog.edn"
    {:handler       #(re-frame/dispatch [:process-infolog-problems %1])
     :error-handler #(re-frame/dispatch [:bad-response %1])})
   (assoc db :loading-problems-csv? true)))

(re-frame/register-handler
 :set-active-page
 re-frame/debug
 (fn [db [_ active-panel]]
   (assoc db :active-page active-panel)))
