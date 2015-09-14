(ns infolog.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [infolog.components.problem-by-module :refer [histogram]]
            [infolog.components.module-dependencies :refer [dependency-graph]]
            [infolog.components.indentation-analysis :refer [complexity-viz]]
            [infolog.routes :refer [page navigation]]
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
      ;;      (logp @problems)
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



(defn problems-view []
  (fn [_]
    [:div

     [pie-problems]
     [histogram]
     [problem-table]]))

(defn dependencies-view []
  [dependency-graph])

(defn indentation-view []
  [complexity-viz])

(defn nesting-row [{:keys [module predicate arity depth calls-in-body]}]
  [:tr
   [:td module]
   [:td predicate]
   [:td arity]
   [:td depth]
   [:td calls-in-body]])

(defn nesting-view []
  (let [nesting (re-frame/subscribe [:nesting])]
    [:table.table
     [:thead
      [:tr
       [:th "Module"]
       [:th "Predicate"]
       [:th "Arity"]
       [:th "Nesting-Depth"]
       [:th "Calls in Body"]]]
     (into [:tbody]
           (mapv nesting-row (reverse (sort-by :depth
                                               (sort-by :calls-in-body (remove (fn [row]
                                                                                 (and (< (:depth row) 5)
                                                                                      (< (:calls-in-body row) 25))) @nesting))))))]))

;; Remember to add page to infolog.routes
(defmethod page :Problems [] [problems-view])
(defmethod page :Dependencies [] [dependencies-view])
(defmethod page :Indentation [] [indentation-view])
(defmethod page :AST-Nesting [] [nesting-view])
(defmethod page :default [] [:h1 "Unknown page"])



(defn render-navigation [active navigation]
  (for [t navigation]
    (if (keyword? t)
      [:li (when (= active t) {:class "active"})
       [:a {:href (str "#/" (name t))} (name  t)]]
      (let [[section & content] t]
        [:li.dropdown
         [:a.dropdown-toggle {:href="#" :data-toggle "dropdown" :role "button"} section [:span.caret]]
         (into [:ul.dropdown-menu] (render-navigation active content))
         ]))))


(defn main-panel []
  (fn []
    (let [location (re-frame/subscribe [:location])
          active (re-frame/subscribe [:active-page])]
      [:div
       [:nav.navbar.navbar-inverse.navbar-fixed-top
        [:div.container
         (into  [:ul.nav.navbar-nav]
                (render-navigation active navigation))]]
       [:div.content
        [:h1 (if (keyword @active) (name @active) "Unknown Page")]
        [:div (str "Directory: " @location)]
        (page @active)]])))


#_(defn main-panel []
    (fn []
      (let [compl (re-frame/subscribe [:call-complexity])
            modcompl (group-by ffirst @compl)]
        [:div
         (into [:ul]
               (map (fn [[m x]]
                      [:li (str m)
                       (into [:ul]
                             (map (fn [[[_ p a] ct]] [:li (str p "/" a " -> " ct)]) x))]) modcompl))])))
