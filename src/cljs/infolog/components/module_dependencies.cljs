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

(defn sort-modules-by-deps [deps in out]
  (let [a (when out (frequencies (map first deps)))
        b (when in (frequencies (map second deps)))
        c (merge-with + a b)]
    (fn [m] (get c m 0))))




(defmulti sort-modules (fn [a _] a))
(defmethod sort-modules :inout-deps [_ deps] (sort-modules-by-deps deps true true))
(defmethod sort-modules :in-deps [_ deps] (sort-modules-by-deps deps true false))
(defmethod sort-modules :out-deps [_ deps] (sort-modules-by-deps deps false true))
(defmethod sort-modules :default [_ _] identity)

(defn dependency-matrix []
  (let [deps (re-frame/subscribe [:dependencies])
        module-sorting (re-frame/subscribe [:dep-sort-modules])
        modules (re-frame/subscribe [:modules (sort-modules @module-sorting @deps)])
        modulesv (reaction (into [] @modules))
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
                       :on-click (fn [] (js/alert (str (get @modulesv x) ":" (get @modulesv y))))
                       :fill (compute-color @deps @modulesv x y)}]))
      (doall (for [y (range 0 @size)]
               [:text {:class "dep-label" :x 0 :y (+ 10 (* cz y))} (get @modulesv y)]))
      (doall (for [y (range 0 @size)]
               [:text {:class "dep-label" :x (* @size cz) :y (- (+ 201 (* cz y))) :transform "rotate(90)"} (get @modulesv y)]  ))]]
    ))

(defn mk-label [c k l]
  [:label.radio-inline
   [:input {:type "radio"
            :name "module-sorting"
            :checked (when (= c k) "checked")
            :on-change (fn [e]
                         (re-frame/dispatch
                          [:switch-dep-module-sort k]))}] l])

(defn dependency-graph []
  (let [selected (re-frame/subscribe [:dep-sort-modules])]
    [:div.panel.panel-default
     [:div.panel-body
      [:form.form-horizontal
       (mk-label @selected :inout-deps "In+Out")
       (mk-label @selected :in-deps "In")
       (mk-label @selected :out-deps "Out")
       [:label.radio-inline
        [:input {:type "radio"
                 :name "module-sorting"
                 :checked (when-not @selected "checked")
                 :on-change (fn [e]
                              (re-frame/dispatch
                               [:switch-dep-module-sort nil]))}] "Alphabetical"]]
      [dependency-matrix]]]))
