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

(def focus (r/atom "root"))
(def hover-text (r/atom ""))

(defn translate
  ([a] (translate a a))
  ([a b]
   (str "translate(" a "," b ")")))


(declare mk-inner-node)

(defn create-structure [top content]
  (let [leafs (filter #(empty? (:path %)) content)
        inner-nodes (remove #(empty? (:path %)) content)
        ign (group-by (fn [n] (first (:path n))) inner-nodes)]
    {:name top :inner-node true :root (= top "root") :children (into leafs (map mk-inner-node ign))}))


(defn mk-inner-node [[top nodes]]
  (let [nodes (map (fn [e] (assoc e :path (rest (:path e)))) nodes)]
    (create-structure top nodes)))

(defn make-module-db [modules]
  (into {} (map (fn [{:keys [name] :as e}] [name e]) modules)))

(defn join-data [max-depth mods data]
  (map (fn [[_ clauses]]
         (let [size (reduce + (map :calls-in-body clauses))
               depth (apply max (map :depth clauses))
               weight (double (/ depth max-depth))
               module-name (:module (first clauses))
               module-path (conj (vec (:path (mods module-name))) module-name)]
           {:module module-name
            :path module-path
            :name (:pa (first clauses))
            :size size :weight weight :children []}))
       (group-by :mpa data)))

(defn mk-data [modules data]
  (let [mods (make-module-db @modules)
        max-depth (apply max (map :depth @data))
        joined (join-data max-depth mods @data)
        nodes (create-structure "root" joined)]
    (reaction nodes)))

(defn zoom [node]
  (let [r (aget node "r")
        x (aget node "x")
        y (aget node "y")
        k (/ inner-diameter r 2)]
    (.domain x-scale #js [(- x r) (+ x r)])
    (.domain y-scale #js [(- y r) (+ y r)])
    (.. js/d3 -event (stopPropagation))
    (.. js/d3
        (selectAll "circle")
        (transition)
        (duration 500)
        (attr "r" (fn [d] (when d (let [r (aget d "r")] (* r k)))))
        (attr "transform" (fn [d] (when d (let [x (aget d "x") y (aget d "y")] (translate (x-scale x) (y-scale y)))))))))

(defn update-fn [d]
  (logp :update-viz)
  (let [dse js/d3
        g (.select dse ".viz-g")
        t (.select dse ".viz-t")
        nodes (.nodes pack (clj->js d))
        points (.. g
                   (selectAll "circle")
                   (data nodes))]
    (.. points
        (enter)
        (append "circle")
        (attr "id" (fn [d] (aget d "name")))
        (attr "class" (fn [d] (str "nesting-node" (when (aget d "inner-node") " nesting-inner-node"))))
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
        (on "click" (fn [d]
                      (if (aget d "inner-node")
                        (zoom d)
                        (let [d (aget d "parent")] (zoom d)))))
        (on "mouseover" (fn [d]
                          (let [name (aget d "name")
                                name (if (= "root" name) "" name)
                                module (aget d "module")
                                x (aget d "x")
                                y (aget d "y")]
                            (reset! hover-text [(if module (str module ":" name) name) x y]))))
        (on "mouseout" (fn [_] (reset! hover-text ["" 0 0]))))))

(defn mount-fn [rc data]
  (logp :mounting-viz)
  (let [el (.getDOMNode rc)
        svg (.. js/d3
                (select el)
                (append "svg")
                (attr "width" diameter)
                (attr "height" diameter))]
    (.. svg
        (append "g")
        (attr "class" "viz-g")
        (attr "transform" (translate margin)))

    (.. svg
        (append "g")
        (attr "class" "viz-t"))

    (update-fn data)))

(defn viz []
  (let [modules (re-frame/subscribe [:raw-modules])
        data (mk-data modules (re-frame/subscribe [:nesting]))]
    (r/create-class
     {:component-did-mount (fn [rc] (mount-fn rc @data))
      :component-did-update (fn [_] (update-fn @data))
      :reagent-render (fn [_]
                        [:div
                         [:div.nesting-viz {:data-focus @focus :data-count (count @data)}]])})))

(defn hover []
  (let [[text x y] @hover-text]
    [:div#hover {:style {:position "absolute"
                         :background  "rgba(238, 237, 217,0.75)"
                         :left (str x "px")
                         :top (str y "px")}} text]))

(defn nesting-viz []
  [:div
   [hover]
   [viz]])
