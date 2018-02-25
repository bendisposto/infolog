(ns infolog.components.indentation-analysis
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [infolog.ui-components :as ui]
            [taoensso.encore :as enc  :refer (logf log logp)]))



(defn transform-freq [[k v]]
  {:indentation k :occurrences v})


(defn complex-row [mods]
  (fn [idx {:keys [avg max file freqs] :as x}]
    (let [f (str file)]
      [:tr
       [:td (.toFixed avg 2)]
       [:td max]
       [:td (get mods f f)]
       [:td (if (< idx 10) [ui/bar-chart (map transform-freq freqs)] [:div "skipped"])]])))

(defn complexity-viz []
  (let [modules (re-frame/subscribe [:module-lookup])
        complexity (re-frame/subscribe [:complexity])]
    [:table.table
     [:thead
      [:tr
       [:th "Average Indentation Level"]
       [:th "Maximum Indentation Level"]
       [:th "Module/Filename"]
       [:th "Histogram"]]]
      (into [:tbody]
           (map-indexed (complex-row @modules) (reverse  (sort-by :max @complexity))))
           ]))
