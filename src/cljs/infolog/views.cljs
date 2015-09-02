(ns infolog.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [cljsjs.c3]))

;; --------------------



(defn home-title []
  (fn []
    [:h1 (str "Infolog Problems")]))

(defn fix-wrap [entry]
  (clojure.string/replace entry #"," ", "))

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

(defn pie-problems [_]
  (let [problems (re-frame/subscribe [:problems])]
    (r/create-class
     {:component-did-mount
      (fn [e]
        (let [data (frequencies (map :problem-type @problems))
              chart (clj->js {:bindto "#pie_problems" :data {:columns (seq data)
                                                             :type "pie"}})]
          (logp (seq data))
          (. js/c3 generate chart)))
      :reagent-render (fn [_] [:div#pie_problems])})))

(defn home-panel []
  (let [location (re-frame/subscribe [:location])]
    (fn [_]
      [:div
       [home-title]
       [:div "Directory: " @location]
       [pie-problems]
       [problem-table]])))


;; --------------------
(defmulti panels identity)
(defmethod panels :home-panel [] [home-panel])
(defmethod panels :default [] [:div])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [:active-panel])]
    (fn []
      (panels @active-panel))))
