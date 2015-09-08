(ns infolog.components.module-dependencies
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [depsd3]))

(def cz 10)
(def offset 200)

(defn compute-color [deps modules x y]
  (let [m1 (get modules x)
        m2 (get modules y)]
    (cond (= m1 m2) "white"
          (and (get deps [m1 m2]) (get deps [m2 m1])) "red"
          (get deps [m1 m2]) "green"
          (get deps [m2 m1]) "blue"
          :else "white")))

(defn dependency-graph []
  (let [deps (re-frame/subscribe [:dependencies])
        modules (reaction (into [] @(re-frame/subscribe [:modules identity])))
        size (reaction (count @modules))
        mx (re-frame/subscribe [:modules count])]
    [:div
     #_[:svg {:height 200 :with 1000}
        [:text {:class "dep-label"
                :x 3
                :y -20
                :stroke "black"
                :transform "rotate(90)"} (last @mx)]]
     [:svg {:height (+ (* @size cz) offset) :width (+ (* @size cz) offset)}
      (doall (for [x (range 0 @size) y (range 0 @size)]
               [:rect {:key (str x "," y)
                       :x (+ (* x cz) offset)
                       :y (+ (* y cz) 0)
                       :stroke-width "0.1px"
                       :stroke "black"
                       :width cz
                       :height cz
                       :on-click (fn [] (js/alert (str (get @modules x) ":" (get @modules y))))
                       :fill (compute-color @deps @modules x y)}]))
      (doall (for [y (range 0 @size)]
               [:text {:class "dep-label" :x 0 :y (+ 10 (* cz y))} (get @modules y)]))
      (doall (for [y (range 0 @size)]
               [:text {:class "dep-label" :x (* @size cz) :y (- (+ 201 (* cz y))) :transform "rotate(90)"} (get @modules y)]  ))]]
    ))
