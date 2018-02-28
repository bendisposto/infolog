(ns infolog.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [infolog.components.problem-by-module :refer [histogram]]
            [infolog.components.module-dependencies :refer [dependency-graph]]
            [infolog.components.indentation-analysis :refer [complexity-viz]]
            [infolog.components.viz-nesting :refer [nesting-viz]]
            [infolog.components.viz-halstead :refer [halstead-viz]]
            [infolog.components.viz-interface-size :refer [interface-size-viz]]
            [infolog.components.viz-dynamics :refer [dynamics-viz]]
            [infolog.components.viz-incalls :refer [incalls-viz]]
            [infolog.routes :refer [page navigation text]]
            [cljs.pprint :as pprint]
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

(defn nesting-row [{:keys [module predicate arity depth calls-in-body start end]}]
  [:tr
   [:td module]
   [:td predicate]
   [:td arity]
   [:td depth]
   [:td calls-in-body]
   [:td (+ 1 (- end start))]
   ])

(defn nesting-view []
  (let [nesting (re-frame/subscribe [:nesting])]
    [:table.table
     [:thead
      [:tr
       [:th "Module"]
       [:th "Predicate"]
       [:th "Arity"]
       [:th "Nesting-Depth"]
       [:th "Calls in Body"]
       [:th "Lines of code"]]]
     (into [:tbody]
           (mapv nesting-row (reverse (sort-by :depth
                                               (sort-by :calls-in-body (remove (fn [row]
                                                                                 (and (< (:depth row) 5)
                                                                                      (< (:calls-in-body row) 25))) @nesting))))))]))

(defn unif-row [{:keys [module predicate arity variables unifications explicit-unifications]}]
  [:tr
   [:td module]
   [:td predicate]
   [:td arity]
   [:td variables]
   [:td unifications]
   [:td explicit-unifications]
   ])

(defn unif-view []
  (let [nesting (re-frame/subscribe [:nesting])]
    [:table.table
     [:thead
      [:tr
       [:th "Module"]
       [:th "Predicate"]
       [:th "Arity"]
       [:th "Variables in Body"]
       [:th "Unifications"]
       [:th "Explicit Unifications"]]]
     (into [:tbody]
           (mapv unif-row (reverse (sort-by :unifications
                                               (sort-by :variables (remove (fn [row]
                                                                                 (and (< (:variables row) 10)
                                                                                      (< (:unifications row) 25))) @nesting))))))]))

(defn predstats-row [{:keys [module dynamics-declared public-preds exported-preds]}]
  [:tr
    [:td module]
    [:td dynamics-declared]
    [:td public-preds]
    [:td exported-preds]])

(defn predstats-view []
  (let [predstats (re-frame/subscribe [:predstats])]
    [:table.table
     [:thead
      [:tr
       [:th "Module"]
       [:th "Dynamic predicates"]
       [:th "Public predicates"]
       [:th "Exported predicates"]]]
     (into [:tbody]
           (mapv predstats-row (reverse (sort-by :dynamics-declared @predstats))))]))

(defn calls-row [{:keys [module predicate incalls outcalls]}]
  [:tr
    [:td module]
    [:td predicate]
    [:td incalls]
    [:td outcalls]])

(defn calls-view []
  (let [calls (re-frame/subscribe [:calls])]
    [:table.table
      [:thead
        [:tr
          [:th "Module"]
          [:th "Predicate"]
          [:th "Incoming edges"]
          [:th "Outgoing edges"]]]
      (into [:tbody] (mapv calls-row (reverse (sort-by :incalls @calls))))]))

(defn halstead-row [{:keys [module predicate operator-occ operand-occ distinct-operators distinct-operands volume length vocabulary level difficulty intelligent-content time effort]}]
  (let [format (fn [f] (pprint/cl-format nil "~,2f" f))]
  [:tr
    [:td module]
    [:td predicate]
    [:td operator-occ]
    [:td operand-occ]
    [:td distinct-operators]
    [:td distinct-operands]
    [:td length]
    [:td vocabulary]
    [:td (format volume)]
    [:td (format level)]
    [:td (format difficulty)]
    [:td (format intelligent-content)]
    [:td (format effort)]
    [:td (str (format time) " s")]
    ]))

(defn halstead-view []
  (let [halstead (re-frame/subscribe [:halstead])
        effort (fn [row] (/ (* (:distinct-operators row) (:operand-occ row) (+ (:operand-occ row) (:operator-occ row)) (/ (Math/log (+ (:distinct-operators row) (:distinct-operands row))) (Math/log 2))) (* 2 (:distinct-operands row))))]
    [:table.table
      [:thead
        [:tr
          [:th "Module"]
          [:th "Predicate"]
          [:th "Operator tokens"]
          [:th "Operand tokens"]
          [:th "Distinct operators"]
          [:th "Distinct operands"]
          [:th "Length"]
          [:th "Vocabulary"]
          [:th "Volume"]
          [:th "Level estimator"]
          [:th "Difficulty"]
          [:th "Intelligent Content"]
          [:th "Programming Effort"]
          [:th "Programming Time"]]]
      (into [:tbody] (mapv halstead-row (reverse (sort-by effort (remove (fn [row] (or (< (effort row) 10000) (< (:distinct-operands row) 5) (< (:distinct-operators row) 5))) @halstead)))))]))

;; Remember to add page to infolog.routes
(defmethod page :Problems [] [problems-view])
(defmethod page :Dependencies [] [dependencies-view])
(defmethod page :Indentation [] [indentation-view])
(defmethod page :AST-Nesting [] [nesting-view])
(defmethod page :Unifications [] [unif-view])
(defmethod page :PredStats [] [predstats-view])
(defmethod page :Calls [] [calls-view])
(defmethod page :Halstead [] [halstead-view])
(defmethod page :Viz-Nesting [] [nesting-viz])
(defmethod page :Viz-InterfaceSize [] [interface-size-viz])
(defmethod page :Viz-Dynamics [] [dynamics-viz])
(defmethod page :Viz-Halstead [] [halstead-viz])
(defmethod page :Viz-Incalls [] [incalls-viz])
(defmethod page :default [] [:h1 "Unknown page"])

(defn render-navigation [active navigation]
  (for [t navigation]
    (if (keyword? t)
      [:li (when (= active t) {:class "active"})
       [:a {:href (str "#/" (name t))} (text  t)]]
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
        [:h1 (if (keyword @active) (text @active) "Unknown Page")]
        [:div (str "Directory: " @location)]
        (page @active)]])))



