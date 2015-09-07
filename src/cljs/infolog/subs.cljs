(ns infolog.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [taoensso.encore :as enc  :refer (logf log logp)]))

(re-frame/register-sub
 :name
 (fn [db]
   (reaction (:name @db))))

(re-frame/register-sub
 :active-panel
 (fn [db _]
   (reaction (:active-panel @db))))

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
                      "aggregate" "tcltk" "heaps" })

(re-frame/register-sub
 :modules
 (fn [db]
   (reaction (remove (fn [[m f]] (sicstus-module m)) (:modules @db)))))


(re-frame/register-sub
 :dependencies
 (fn [db]
   (let [modules (re-frame/subscribe [:modules])]
     (reaction (->> (:dependencies @db)
                    (remove (fn [[m d]] (or (sicstus-module m) (sicstus-module d))))
                    (group-by first)
                    (map (fn [[k v]] [k (map second v)]))
                    (into {})
                    (merge (into {} (map vector (map first @modules) (repeat [])))))))))

(re-frame/register-sub
 :inverse-dependencies
 (fn [db]
   (let [modules (re-frame/subscribe [:modules])]
     (reaction (->> (:dependencies @db)
                    (map (fn [[k v]] [v k]))
                    (remove (fn [[m d]] (or (sicstus-module m) (sicstus-module d))))
                    (group-by first)
                    (map (fn [[k v]] [k (map second v)]))
                    (into {})
                    (merge (into {} (map vector (map first @modules) (repeat [])))))))))

(re-frame/register-sub
 :raw-dependencies
 (fn [db]
   (reaction (:dependencies @db))))

(re-frame/register-sub
 :histo-by-module-selected
 (fn [db]
   (reaction (get-in @db [:histo-by-module :show] #{}))))
