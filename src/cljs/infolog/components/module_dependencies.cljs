(ns infolog.components.module-dependencies
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]))

(def cz 10)
(def offset 210)

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

(defn grid [modules size]
  [:g#grid
   (into [:g#columns]
         (doall (for [c (range 0 size)]
                  [:g
                   [:text {:class "dep-label" :x (+ 5 (* size cz)) :y (- (+ offset 2 (* cz c))) :transform "rotate(90)"} (get modules c)]
                   [:rect {:x (+ offset (* c cz)) :y 0 :width cz :height (+ offset (* size cz)) :stroke-width "0.1px" :stroke "black" :fill-opacity 0}]])))
   (into [:g#rows]
         (doall (for [c (range 0 size)]
                  [:g
                   [:text {:class "dep-label" :x 5 :y (+ 8 (* cz c))} (get modules c)]
                   [:rect {:x 0 :y (* c cz) :height cz :width (+ offset (* size cz)) :stroke-width "0.1px" :stroke "black" :fill-opacity 0}]])))])

(defn extract-deps [deps mods]
  (remove nil? (mapcat (fn [[a b]]
                         (let [pa (mods a)
                               pb (mods b)]
                           (when (and pa pb)
                             [{:a a :b b :col "green" :pos [pa pb]}
                              {:a a :b b :col "blue" :pos [pb pa]}]))) deps)))

(defn mk-cell [x y e]
  (let [xp (+ (* x cz) offset)
        yp (+ (* y cz) 0)
        col (cond (= x y) "white"
                  (= 2 (count e)) "red"
                  :else (:col (first e)))]
    [:rect {:key (str x "," y)
            :x xp
            :y yp
            :stroke-width "0.1px"
            :stroke "black"
            :width cz
            :height cz
            :fill col}]))

(defn sparse-dependency-matrix []
  (let [deps (re-frame/subscribe [:dependencies])
        module-sorting (re-frame/subscribe [:dep-sort-modules])
        modules (re-frame/subscribe [:modules (sort-modules @module-sorting @deps)])
        mv (reaction (vec @modules))
        size (reaction (count @modules))
        modules (reaction (into {} (map vector @modules (range))))
        dz (reaction (group-by :pos (extract-deps @deps @modules)))]
    [:div
     [:svg
      {:height (+ (* @size cz) offset)
       :width (+ (* @size cz) offset)}
      [:g
       [grid @mv @size]
       (for [[[x y] e] @dz] (mk-cell x y e))
       [:rect {:x 0
               :y 0
               :width (+ (* @size cz) offset)
               :height (+ (* @size cz) offset)
               :opacity 0
               :on-click (fn [e] (let [t (.-target e)
                                      box (.getBoundingClientRect t)
                                      cx (- (.-clientX e) (.-left box) offset)
                                      cy (- (.-clientY e) (.-top box))
                                      x (int (/ cx cz))
                                      y (int (/ cy cz))
                                      m1 (@mv x)
                                      m2 (@mv y)]
                                  (logp :click [x y] m1 m2 )))}]]]]))

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
    [:div
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
                                [:switch-dep-module-sort nil]))}] "Alphabetical"]]]]
     [sparse-dependency-matrix]]))
