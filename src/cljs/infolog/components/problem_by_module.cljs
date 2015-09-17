(ns infolog.components.problem-by-module
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [cljsjs.d3]))

;; --------------------

(defn update-d3 [el configuration ds]
  (let [{:keys [inner-h inner-w x-axis-g y-axis-g]} @configuration
        g (.. js/d3 (select ".g-container"))
        point (.. g (selectAll ".module-info") (data (clj->js (:freqs ds))))
        c-scale (.. js/d3 -scale (category20c))
        y-scale (.. js/d3 -scale (linear) (domain (clj->js[0 (:maxY ds)])) (range (clj->js[inner-h 0])))
        x-scale (.. js/d3 -scale (ordinal) (rangeBands (clj->js [0 inner-w]) 0.1) (domain (clj->js (map :module (:freqs ds)))))
        x-axis (.. js/d3 -svg (axis) (scale x-scale) (orient "bottom"))
        y-axis (.. js/d3 -svg (axis) (scale y-scale) (orient "left"))]
    (.. x-axis-g (call x-axis) (selectAll "text") (attr "transform" "rotate(90) translate(60,-15)"))
    (.call y-axis-g y-axis)
    (.. point (exit) (remove))
    (.. point
        (enter)
        (append "rect")
        (attr "class" "module-info"))
    (.. point
        (attr "width" (.rangeBand x-scale))
        (attr "height" (fn [d] (let [v (aget d "problems")
                                    y (y-scale v)] (- inner-h y))))
        (attr "y" (fn [d] (let [v (aget  d "problems")
                               y (y-scale v)] y)))
        (attr "fill" (fn [d] (c-scale (aget d "module"))))
        (attr "x" (fn [d] (let [m (aget d "module")
                                x (x-scale (clj->js m))] x))))))

(defn create-d3 [el configuration data]
  (let [{:keys [width height inner-h inner-w left top]} @configuration
        svg (.. js/d3 (select el) (append "svg") (attr "width" width) (attr "height" height))
        r (.. svg (append "rect") (attr "width" width) (attr "height" height) (attr "fill" "#DDFFFF"))
        g (.. svg (append "g")
              (attr "class" "g-container")
              (attr "transform" (str "translate(" left "," top ")")))
        x-axis-g (.. g (append "g") (attr "transform" (str "translate(" 0 "," inner-h ")") ))
        y-axis-g (.. g (append "g"))]
    (swap! configuration assoc :x-axis-g x-axis-g :y-axis-g y-axis-g)
    (update-d3 el configuration data)))

(defn histogram-chart []
  (let [selected (re-frame/subscribe [:histo-by-module-selected])
        problems (re-frame/subscribe [:problems])
        configuration (r/atom {:width 960 :height 450 :top 10 :bottom 150 :left 30 :right 10})
        data (reaction (let [freqs (map (fn [[k v]] {:module k :problems v})
                                        (frequencies
                                         (map :module
                                              (filter #(@selected (:problem-type %))
                                                      @problems))))
                             maxY (apply max (map :problems freqs))]
                         {:freqs freqs :maxY maxY}))]
    (swap! configuration (fn [{:keys [width height left top right bottom] :as c}] (assoc c :inner-w (- width left right) :inner-h (- height top bottom))))
    (r/create-class
     {:component-did-mount
      (fn [rc] (let [el (.getDOMNode rc)]
                (logp :mounting)
                (create-d3 el configuration @data)))
      :component-did-update
      (fn [rc] (let [el (.getDOMNode rc)]
                (logp :update)
                (update-d3 el configuration @data)))
      :reagent-render (fn [x]
                        [:div.histogram {:data-problems (count @problems)
                                         :data-selected (count @selected)}])})))


(defn histogram []
  (fn []
    (let [selected (re-frame/subscribe [:histo-by-module-selected])]
      [:div.panel.panel-default
       [:div.panel-body
        [:h2 "Messages by module"]
        [:form.form-horizontal
           [:label.checkbox-inline
            [:input {:checked (when (@selected "error") "checked")
                     :type "checkbox" :on-change (fn [e] (re-frame/dispatch [:histo-by-module-switch "error" (.. e -target -checked)]))}]
            "Errors"]
           [:label.checkbox-inline
            [:input {:checked (when (@selected "warning") "checked") :type "checkbox" :on-change (fn [e] (re-frame/dispatch [:histo-by-module-switch "warning" (.. e -target -checked)]))}]
            "Warnings"]
           [:label.checkbox-inline
            [:input {:checked (when (@selected "info") "checked"):type "checkbox" :on-change (fn [e] (re-frame/dispatch [:histo-by-module-switch "info" (.. e -target -checked)]))}]
            "Infos"]]
        [histogram-chart]]])))
