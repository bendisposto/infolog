(ns infolog.components.complexity
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [cljsjs.d3]
            [cljsjs.dimple]
            [taoensso.encore :as enc  :refer (logf log logp)]))



(defn- draw-chart [data div {:keys [id chart]}]
  (let [{:keys [width height]}    div
        {:keys [bounds plot
                x-axis y-axis]}   chart
                Chart                     (.-chart js/dimple)
                svg                       (.newSvg js/dimple (str "#" id) width height)
                dimple-chart              (.setBounds (Chart. svg) (:x bounds) (:y bounds) (:width bounds) (:height bounds))
                x                         (.addCategoryAxis dimple-chart "x" x-axis)
                y                         (.addMeasureAxis dimple-chart "y" y-axis)
                s                         (.addSeries dimple-chart nil plot (clj->js [x y]))]
    (aset s "data" (clj->js data))
    (.draw dimple-chart)))


(def data2 [{:value 240000 :timestamp "2014-01-01"}
             {:value 260000 :timestamp "2014-02-01"}
             {:value 290000 :timestamp "2014-03-01"}
             {:value 70000  :timestamp "2014-04-01"}
             {:value 100000 :timestamp "2014-05-01"}
             {:value 120000 :timestamp "2014-06-01"}
             {:value 240000 :timestamp "2014-07-01"}
             {:value 220000 :timestamp "2014-08-01"}
             {:value 360000 :timestamp "2014-09-01"}
             {:value 260000 :timestamp "2014-10-01"}
             {:value 250000 :timestamp "2014-11-01"}
             {:value 190000 :timestamp "2014-12-01"}])


(defn bar-chart [data]
  (let [width 300
        height 150
        id (gensym "chart")]
    (r/create-class
     {:reagent-render (fn [_] [:div {:id id :width width :height height}])
      :component-did-mount (fn [e]
                             (let [node (.getDOMNode e)]
                               (draw-chart data node {:id id
                                                       :chart {:bounds {:x "20%"
                                                                        :y "15%"
                                                                        :width "70%"
                                                                        :height "60%"}
                                                               :plot js/dimple.plot.bar
                                                               :x-axis "indentation"
                                                               :y-axis "occurrences"}})
                                    ))})))

(defn transform-freq [[k v]]
  {:indentation k :occurrences v})


(defn complex-row [mods {:keys [avg max file freqs] :as x}]
  (let [f (str file)]
    [:tr
     [:td (.toFixed avg 2)]
     [:td max]
     [:td (get mods f f)]
     [:td [bar-chart (map transform-freq freqs)]]]))

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
