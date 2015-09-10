(ns infolog.components.complexity
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [cljsjs.d3]
            [cljsjs.dimple]
            [taoensso.encore :as enc  :refer (logf log logp)]))



(defn bar-chart [data-map]
  (let [id (gensym "chart")
        sel (str "#" id)]
    (r/create-class
     {:reagent-render (fn [_]
                        [:div {:id id}])
      :component-did-mount (fn [e]
                             (let [node (.getDOMNode e)] (log node)))}
     )))

#_(defn bar-chart [data-map]
  (let [id (gensym "chart")
        sel (str "#" id)]
    (r/create-class
     {:component-did-mount (fn [_]
                              (.. js/d3
                                  (select sel)
                                  (selectAll "div")
                                  (data (clj->js (seq data-map)))
                                  (enter)
                                  (append "div")
                                  (style "width" (fn [[_ d]] (str (* 10 d) "px")))
                                  (text first)))
      
      :reagent-render (fn [_] [:div {:id id}])})))


(defn complex-row [mods {:keys [avg max file freqs] :as x}]
  (let [f (str file)]
    [:tr
     [:td (.toFixed avg 2)]
     [:td max]
     [:td (get mods f f)]
     [:td [bar-chart freqs]]]))

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
           (map (partial complex-row @modules) (reverse  (sort-by :max @complexity))))]))
