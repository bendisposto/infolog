(ns infolog.components.viz-nesting
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [cljsjs.d3]
            [taoensso.encore :as enc  :refer (logf log logp)]))

(def diameter 960)
(def margin 10)
(def inner-diameter (- diameter margin margin))

(def x-scale (.. js/d3 -scale (linear) (range #js [0 inner-diameter])))
(def y-scale (.. js/d3 -scale (linear) (range #js [0 inner-diameter])))

(def color-scale (.. js/d3 -scale
                     (linear)
                     (domain #js [-1 5])
                     (range (clj->js ["hsl(185,60%,99%)" "hsl(187,40%,70%)"]))
                     (interpolate (aget js/d3 "interpolateHcl"))))

(def pack (.. js/d3 -layout
              (pack)
              (padding 2)
              (size #js [inner-diameter inner-diameter])
              (value (fn [d] (aget d "size")))))

(defn translate
  ([a] (translate a a))
  ([a b]
   (str "translate(" a "," b ")")))


(defn mk-data [modules data]
  (let [max-depth (apply max (map :depth @data))
        joined (map (fn [[_ clauses]]
                      (let [size (reduce + (map :calls-in-body clauses))
                            depth (apply max (map :depth clauses))
                            weight (double (/ depth max-depth))]
                        {:module (:module (first clauses))
                         :name (:pa (first clauses))
                         :size size :weight weight :children []}))
                    (group-by :mpa @data))
        g (group-by :module joined)
        x (map (fn [[module predicates]] {:name module :children predicates}) g)]
    (reaction {:name "root"
               :children x})))


(defn update-fn [d]
  (logp :update-viz)
  (let [dse js/d3
        g (.select dse ".viz-g")
        nodes (.nodes pack (clj->js d))
        points (.. g
                   (selectAll "circle")
                   (data nodes))]
    (.. points
        (enter)
        (append "circle")
        (attr "class" (fn [d]  "node"))
        (attr "r" (fn [d] (aget d "r")))
        (attr "fill" (fn [d]
                       (let [w (aget d "weight")
                             children? (aget d "children")
                             depth (aget d "depth")]
                         (cond (pos? w) "darkred"
                               children? (color-scale depth)
                               :otherwise "WhiteSmoke"))))
        (attr "fill-opacity" (fn [d] (aget d "weight")))
        (attr "transform" (fn [d] (translate (aget d "x") (aget d "y"))))
        )))

(defn mount-fn [rc data]
  (logp :mounting-viz)
  (let [el (.getDOMNode rc)
        g (.. js/d3
              (select el)
              (append "svg")
              (attr "width" diameter)
              (attr "height" diameter)
              (append "g")
              (attr "class" "viz-g")
              (attr "transform" (translate margin)))]
    (update-fn data)))

(defn nesting-viz []
  (let [modules (re-frame/subscribe [:modules])
        data (mk-data modules (re-frame/subscribe [:nesting]))
        focus (r/atom "root")]
    (r/create-class
     {:component-did-mount (fn [rc] (mount-fn rc @data))
      :component-did-update (fn [_] (update-fn @data))
      :reagent-render (fn [_]
                        [:div.nesting-viz {:data-count (count @data)}])})))
