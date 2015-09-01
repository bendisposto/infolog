(ns infolog.views
    (:require [re-frame.core :as re-frame]
              [re-com.core :as re-com]
              [cljsjs.csv :as csv]))

;; --------------------


(defn print-csv []
  (let [content "Category,Type,Message,Module,Pred,File,L1,L2,Hash\n\"infolog_internal_error\",error,\"analyze body failed: analyze_body(call(_92361#=_92363),[87,[87,87,87]],chr_integer_inequality:setup_eq/2,no_dcg)\",chr_integer_inequality,setup_eq/2,\"/Users/leuschel/git_root/prob_prolog/src/chr/chr_integer_inequality.pl\",87,87,_33177"
        parsed (. js/CSV parse content)]
        (clojure.string/join "," (map count parsed))))

(defn home-title []
  (let [name (re-frame/subscribe [:name])]
    (fn []
      [re-com/title
       :label (str "Hello from: " @name)
       :level :level1])))


(defn link-to-about-page []
  [re-com/hyperlink-href
   :label "go to About Page"
   :href "#/about"])

(defn home-panel []
  [re-com/v-box
   :gap "1em"
   :children
   [[home-title]
   [:p (print-csv)]
   [link-to-about-page]]])

;; --------------------
(defn about-title []
  [re-com/title
   :label "This is the About Page."
   :level :level1])

(defn link-to-home-page []
  [re-com/hyperlink-href
   :label "go to Home Page"
   :href "#/"])

(defn about-panel []
  [re-com/v-box
   :gap "1em"
   :children [[about-title] [link-to-home-page]]])

;; --------------------
(defmulti panels identity)
(defmethod panels :home-panel [] [home-panel])
(defmethod panels :about-panel [] [about-panel])
(defmethod panels :default [] [:div])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [:active-panel])]
    (fn []
      [re-com/v-box
       :height "100%"
       :children [(panels @active-panel)]])))
