(ns infolog.components.problem-by-module
  (:require [re-frame.core :as re-frame]
            [reagent.core :as r]
            [taoensso.encore :as enc  :refer (logf log logp)]
            [cljsjs.c3]))

;; --------------------

(defn c3-component [update-function render-function]
  (r/create-class {:component-did-mount update-function
                   :component-did-update update-function
                   :reagent-render render-function}))

(defn histogram []
  (let [selected (re-frame/subscribe [:histo-by-module-selected])
        problems (re-frame/subscribe [:problems])]
    (c3-component
     (fn [_] (when @problems
              (do  (let [data (frequencies (map :module (filter #( @selected (:problem-type %)) @problems)))
                         chart (clj->js {:bindto "#histo_problems"
                                         :legend {:show false}
                                         :data {:columns (reverse (sort-by second (seq data)))
                                                :type "bar"
                                                :labels true}
                                         :tooltip {:grouped false
                                                   :format {:title (fn [_] "")
                                                            :value (fn [v _ n] (str ": " v))}
                                                   }
                                         :axis {:y {:label "#Problems"}}})]
                     (. js/c3 generate chart)))))
     (fn []
       [:div.panel.panel-default
        [:div.panel-body
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
         [:div#histo_problems (count @problems)]]]))))
