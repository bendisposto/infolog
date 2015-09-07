(ns infolog.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [infolog.components.problem-by-module :refer [histogram]]
            [infolog.components.module-dependencies :refer [dependency-graph]]
            [cljsjs.c3]
            [cljsjs.d3]))

;; --------------------



(defn home-title []
  (fn []
    [:h1 (str "Infolog Problems")]))

(defn fix-wrap [entry]
  (-> entry
      (clojure.string/replace #"," ", ")
      (clojure.string/replace #":" ": ")))

(defn render-row [{:keys [hash problem-type] :as m}]
  [:tr {:id (str "hash_" hash)
        :class (cond (= problem-type "error") "danger"
                     (= problem-type "warning") "warning")}
   [:td (:category m)]
   [:td problem-type]
   [:td (fix-wrap (:message m))]
   [:td (:module m)]
   [:td (:predicate m)]
   [:td (:file m)]
   [:td (:start m)]
   [:td (:end m)]])

(defn extract-categories [data]
  (into #{} (map :category data)))

(defn problem-table [data]
  (let [problems (re-frame/subscribe [:problems])]
    (fn [_]
      (conj [:table {:id "problem-table"
                     :class "table"}
             [:thead [:tr
                      [:th "Category"]
                      [:th "Type"]
                      [:th "Message"]
                      [:th "Module"]
                      [:th "Predicate"]
                      [:th "File"]
                      [:th "Start Line"]
                      [:th "End Line"]]]]
            (into [:tbody] (mapv render-row (sort-by (comp {"error" 0 "warning" 1 "info" 2} :problem-type) @problems)))))))

(defn c3-component [update-function render-function]
  (r/create-class {:component-did-mount update-function
                   :component-did-update update-function
                   :reagent-render render-function}))

(defn pie-problems [_]
  (let [problems (re-frame/subscribe [:problems])]
    (c3-component
     (fn [e] (when @problems
              (do (let [data (frequencies (map :problem-type @problems))
                        chart (clj->js {:bindto "#pie_problems" :data {:columns (seq data)
                                                                       :type "pie"
                                                                       :colors {"error" "#cc0000"
                                                                                "warning" "#F3F781"
                                                                                "info" "#5882FA"}}})]
                    (logp :prepared-data data chart)
                    (. js/c3 generate chart)))))
     (fn [_] [:div#pie_problems (count @problems)]))))


(comment (defn update-doh [_]
           (.. js/d3
               (select "#doh")
               (append "svg")
               (attr "width" 500)
               (attr "height" 500)
               (append "g")
               ))
         (defn render-doh [_] [:div#doh])

         (defn doh []
           (let [problems (re-frame/subscribe [:problems])]
             (c3-component update-doh render-doh))))


(defn main-panel []
  (let [location (re-frame/subscribe [:location])]
    (fn [_]
      [:div
       [:h1 "Infolog"]
       [:div "Directory: " @location]
       [:h1 "Dependency Analysis"]
       [dependency-graph]
       [:h1 "Problems"]
       [pie-problems]
       [histogram]
       [problem-table]])))
