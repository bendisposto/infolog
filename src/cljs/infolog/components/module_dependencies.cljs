(ns infolog.components.module-dependencies
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [depsd3]))

(def cz 4)

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
        size (reaction (count @modules))]
    [:svg {:height (* @size cz) :width (* @size cz)}
     (doall (for [x (range 0 @size) y (range 0 @size)]
              [:rect {:key (str x "," y)
                      :x (* x cz)
                      :y (* y cz)
                      :width cz
                      :height cz
                      :fill (compute-color @deps @modules x y)}]))]
    ))
