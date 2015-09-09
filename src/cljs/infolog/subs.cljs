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

(re-frame/register-sub
 :modules
 (fn [db [_ sort-fn]]
   (reaction (let [mods (conj (map first (remove (fn [[m f]] (sicstus-module m)) (:modules @db))) "user")]
               (if sort-fn (sort-by sort-fn mods) mods)))))

(re-frame/register-sub
 :dep-sort-modules
 (fn [db [_]](reaction (:dep-sort-modules @db))))

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
