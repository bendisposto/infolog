(ns infolog.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [taoensso.encore :as enc  :refer (logf log logp)]))

(re-frame/register-sub
 :name
 (fn [db]
   (reaction (:name @db))))

(re-frame/register-sub
 :active-page
 (fn [db _]
   (reaction (:active-page @db))))

(re-frame/register-sub
 :problems
 (fn [db]
   (reaction (:infolog-problems @db))))

(re-frame/register-sub
 :location
 (fn [db]
   (reaction (:directory @db))))

(def sicstus-module #{"lists" "avl" "file_systems" "codesio" "ordsets" "timeout"
                      "chr" "clpfd" "system" "between" "samsort" "random" "atts"
                      "terms" "sets" "gauge" "trees" "assoc" "xml" "process"
                      "aggregate" "tcltk" "heaps" "fastrw" "sockets" "wgraphs" "ugraphs"})

(defn remove-sicstus-modules [modules]
  (remove #(sicstus-module (:name %)) modules))

(re-frame/register-sub
 :modules
 (fn [db [_ sort-fn]]
   (let [own (remove-sicstus-modules (:modules @db))
         _ (logp :m (:modules @db) :own own)
         names (conj (map :name own) "user")]
     (reaction (if sort-fn (sort-by sort-fn names) names)))))

(re-frame/register-sub
 :raw-modules
 (fn [db]
   (reaction (remove (fn [{m :name}] (sicstus-module m)) (:modules @db)))))

(re-frame/register-sub
 :module-lookup
 (fn [db]
   (reaction (clojure.set/map-invert (:modules @db)))))

(re-frame/register-sub
 :module-size
 (fn [db] (reaction (:module-size @db))))

(re-frame/register-sub
 :module-internal-size
 (fn [db]
   (let [r (->> @db
                :nesting
                (map (fn [{:keys [module pa]}] [module pa]))
                (group-by first)
                (map (fn [[m g]] [m (count g)]))
                (into {}))]
     (logp r)
     (reaction r))))


(re-frame/register-sub
 :dep-sort-modules
 (fn [db [_]](reaction (:dep-sort-modules @db))))

(re-frame/register-sub
 :call-complexity
 (fn [db]
   (reaction (:call-complexity @db))))

(re-frame/register-sub
 :nesting
 (fn [db]
   (reaction (:nesting @db))))

(re-frame/register-sub
 :dependencies
 (fn [db]
   (reaction (:dependencies @db))))

(re-frame/register-sub
 :histo-by-module-selected
 (fn [db]
   (reaction (get-in @db [:histo-by-module :show] #{}))))

(re-frame/register-sub
 :selected-dependency
 (fn [db]
   (reaction (:selected-dependency @db))))

(re-frame/register-sub
 :complexity
 (fn [db]
   (reaction (:complexity @db))))
