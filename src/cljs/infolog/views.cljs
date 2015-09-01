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


(defn problem-row [row]
  (into [:tr {:id (str "hash_" (last row))}] (mapv (fn [c] [:td c]) (butlast row))))

(defn home-panel []
  (let [problems (re-frame/subscribe [:problems])]
    (fn [_]
      [re-com/v-box
       :gap "1em"
       :children
       [[home-title]
        (when @problems (into [:table#problem-table.table.table-striped] (mapv problem-row @problems)))]])))


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
