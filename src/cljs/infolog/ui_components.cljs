(ns infolog.ui-components
  (:require [reagent.core :as r]
             [cljsjs.d3]
             [cljsjs.dimple]
             [taoensso.encore :as enc  :refer (logf log logp)]
             ))


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
