(ns infolog.components.module-dependencies
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [depsd3]))

(defn component [update-function render-function]
  (r/create-class {:component-did-mount update-function
                   :component-did-update update-function
                   :reagent-render render-function}))


(defn dep-cell [{:keys [uses used-by]}]
  (fn [index]
    (let [u (get uses index)
          ub (get used-by index)]
      [:td (merge {:title index :width 6}
                  (cond
                    (and u ub) {:class "circular"}
                    u {:class "uses"}
                    ub {:class "used-by"}
                    :otherwise {:class "no-dep"})) " "])))

(defn dep-row [index]
  (fn  [[k vs]]
    (into [:tr [:td k]] (mapv (dep-cell vs) index))))

(defn dependency-graph []
  (let [modules (re-frame/subscribe [:modules])
        deps (re-frame/subscribe [:dependencies])
        inv-deps (re-frame/subscribe [:inverse-dependencies])
        merged' (reaction (merge-with (fn [v1 v2] {:uses (into #{} v1) :used-by (into #{} v2)}) @deps @inv-deps))
        user (reaction (get @merged' "user"))
        merged (if (map? @user)
                  merged'
                  (reaction (assoc @merged' "user" {:uses @user :used_by #{}})))
        index (reaction (conj (map first @modules) "user"))
        ]
    [:div
     [:table.dep-table (into [:tbody] (mapv (dep-row @index) @merged))]]))

#_(defn dependency-graph []
    (let [d (depsd3/dependency)
          dependencies (re-frame/subscribe [:dependencies])
          modules (re-frame/subscribe [:modules])]
      (component
       (fn [_] (when @modules (. d update dependencies)))
       (fn [_]
         [:div
          [:svg#depgraph {:width 1000 :height 1000}]
          [:p (str "#Modules: " (count @modules) ", #Dependencies: " (count @dependencies) " "
                   (count (mapcat second  @dependencies)) )]]))))
