(ns infolog.handlers
  (:require [re-frame.core :as re-frame]
            [infolog.db :as db]
            [ajax.core :as ajax]
            [cljs.reader :as reader]
            [taoensso.encore :as enc  :refer (logf log logp)]))

(re-frame/register-handler
 :initialize-db
 (fn  [_ _]
   (logp :init-db)
   (re-frame/dispatch [:request-infolog-edn])
   (re-frame/dispatch [:request-complexity-edn])
   db/default-db))


(defn split-path [sep path]
  (clojure.string/split path (re-pattern sep)))

(defn common-prefix [sep paths]
  (let [parts-per-path (map (partial split-path sep) paths)
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

(defn extend-with-paths [prefix modules]
  (for [[m f] modules]
    (let [f' (clojure.string/replace-first f prefix "")
          p (butlast (rest (split-path "/" f')))]
      {:name m :file f' :path p})))

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

(defn nesting->map [[m p a d cib sl el vc u eu]]
  {:module m
   :predicate p
   :arity a
   :depth d
   :calls-in-body cib
   :mpa (str m ":" p "/" a)
   :pa  (str p "/" a)
   :start sl
   :end el
   :variables vc
   :unifications u
   :explicit-unifications eu})

(defn predstats->map [[m ds ps es]]
  {:module m
   :dynamics-declared ds
   :public-preds ps
   :exported-preds es})

(defn calls->map [[m p ic oc]]
  {:module m
   :predicate p
   :incalls ic
   :outcalls oc})

(defn halstead->map [[m p sl el otors onds dotors donds]]
  (let [N (+ otors onds)
        n (+ dotors donds)
        V (* N (/ (Math/log n) (Math/log 2)))
        L (* (/ 2 dotors) (/ (+ donds 1) (+ onds 1)))
        D (/ 1 L)
        I (* L V)
        E (/ V L)
        T (/ E 18)]
  {:module m
   :predicate p
   :start sl
   :end el
   :mpa (str m ":" p)
   :operator-occ otors
   :operand-occ onds
   :distinct-operators dotors
   :distinct-operands donds
   :length N
   :vocabulary n
   :volume V
   :level L
   :difficulty D
   :intelligent-content I
   :effort E
   :time T
   }))

(defn transform-calls [calls]
  (map call->map calls))

(defn call-complexity [calls]
  (->> calls
       (remove (fn [[m1 _ _ m2 _ _ _ _]] (= m1 m2)))
       (group-by (partial take 3))
       (map (fn [[k vs]] [k (count vs)]))
       (remove (fn [[[_ p a] b]] (= [p a] [":-" 1])))
       (sort-by second)
       reverse))

(defn module-size [data]
  (->> data
       (group-by first)
       (map (fn [[m g]] [m (count g)]))
       (into {})))

(re-frame/register-handler
 :process-infolog-edn
 (fn [db [_ r]]
   (let [result (if (map? r) r (cljs.reader/read-string r))
         [prefix problems] (transform-problems (:infolog_problem_flat result))
         raw-call (:calling result)
         calling (transform-calls raw-call)
         deps (into #{} (map (fn [{:keys [caller-module callee-module]}] [caller-module callee-module]) calling))
         db' (assoc db
                    :infolog-problems problems
                    :directory prefix
                    :modules (extend-with-paths prefix (:defined_module result))
                    :use-modules (:depends_on result)
                    :dependencies deps
                    :predicates (:predicate result)
                    :module-size (module-size (:is_exported result))
                    :call-complexity (call-complexity raw-call)
                    :nesting (map nesting->map (:clause_complexity result))
                    :predstats (map predstats->map (:module_predicate_stats result))
                    :calls (map calls->map (:pred_incalls_outcalls result))
                    :halstead (map halstead->map (:clause_halstead result))
                    :raw-calls raw-call)]
     db')))

(re-frame/register-handler
 :process-complexity-edn
 (fn [db [_ r]]
   (let [result (if (map? r) r (cljs.reader/read-string r))
         complexity (:complexity result)]
     (assoc db :complexity complexity))))

(re-frame/register-handler
 :bad-response
 (fn [db [_ result]]
   (js/alert "Download failed. See console for details.")
   (logp :error result)
   db))

(re-frame/register-handler
 :histo-by-module-switch
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

(defn request-file [url ok-dispatch error-dispatch]
  (logp :requesting url)
  (ajax/GET
   url
   {:params {:time (.getTime (js/Date.))}
    :handler       #(re-frame/dispatch [ok-dispatch %1])
    :error-handler #(re-frame/dispatch [error-dispatch %1])}))

(re-frame/register-handler
 :request-infolog-edn
 (fn [db _]
   (request-file "infolog.edn" :process-infolog-edn :bad-response)
   db))

(re-frame/register-handler
 :request-complexity-edn
 (fn [db _]
   (request-file "indy.edn" :process-complexity-edn :bad-response)
   db))

(re-frame/register-handler
 :set-active-page
 re-frame/debug
 (fn [db [_ active-panel]]
   (assoc db :active-page active-panel)))

(re-frame/register-handler
 :set-selected-dependency
 re-frame/debug
 (fn [db [_ v]]
   (assoc db :selected-dependency v)))
