(ns infolog.views
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [cljsjs.csv :as csv]))

;; --------------------



(defn home-title []
  (fn []
    [re-com/title
     :label (str "Infolog Problems")
     :level :level1]))

(defn common-prefix [sep paths]
  (let [parts-per-path (map #(clojure.string/split % (re-pattern sep)) paths)
        parts-per-position (apply map vector parts-per-path)]
    (clojure.string/join sep
                         (for [parts parts-per-position :while (apply = parts)]
                           (first parts)))))

(defn file-prefix [data]
  (let [files (remove #{"unknown"} (map #(nth % 5) data))]
    (common-prefix "/" files)))

(defn fix-path [prefix entry]
  (clojure.string/replace entry (js/RegExp. prefix) "."))

(defn fix-wrap [entry]
  (clojure.string/replace entry #"," ", "))

(defn render-row [prefix
                  [category problem-type message module predicate file start end hash]]
  [:tr {:id (str "hash_" hash)
        :class (cond (= problem-type "error") "danger"
                     (= problem-type "warning") "warning")}
   [:td category]
   [:td problem-type]
   [:td (fix-wrap message)]
   [:td module]
   [:td predicate]
   [:td (fix-path prefix file)]
   [:td start]
   [:td end]])

(defn problem-table [data]
  (let [prefix (file-prefix data)]
    (into [:table {:id "problem-table"
                   :class "table table-striped"}
           [:thead [:tr
                    [:th "Category"]
                    [:th "Type"]
                    [:th "Message"]
                    [:th "Module"]
                    [:th "Predicate"]
                    [:th "File"]
                    [:th "Start Line"]
                    [:th "End Line"]]]]
          (mapv (partial render-row prefix) data))))

(defn home-panel []
  (let [problems (re-frame/subscribe [:problems])]
    (fn [_]
      [re-com/v-box
       :gap "1em"
       :children
       [[home-title]
        (when @problems [problem-table @problems])]])))


;; --------------------
(defmulti panels identity)
(defmethod panels :home-panel [] [home-panel])
(defmethod panels :default [] [:div])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [:active-panel])]
    (fn []
      [re-com/v-box
       :height "100%"
       :children [(panels @active-panel)]])))
