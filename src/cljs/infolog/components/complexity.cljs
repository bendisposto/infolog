(ns infolog.components.complexity
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]))

(defn complex-row [mods {:keys [avg max file] :as x}]
  (let [f (str file)]
    [:tr
     [:td (.toFixed avg 2)]
     [:td max]
     [:td (get mods f f)]]))

(defn complexity-viz []
  (let [modules (re-frame/subscribe [:module-lookup])
        complexity (re-frame/subscribe [:complexity])]
    [:table.table
     [:thead
      [:tr
       [:th "Average Indentation Level"]
       [:th "Maximum Indentation Level"]
       [:th "Module/Filename"]]]
     (into [:tbody]
           (map (partial complex-row @modules) (reverse  (sort-by :max @complexity))))]))
