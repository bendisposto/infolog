(ns infolog.routes
    (:require-macros [secretary.core :refer [defroute]])
    (:import goog.History)
    (:require [secretary.core :as secretary]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [re-frame.core :as re-frame]
              [taoensso.encore :as enc  :refer (logf log logp)]))

(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defn text [kw]
  (or ({:Viz-Nesting "AST-Nesting"
        :Viz-InterfaceSize "Interface Size"
        :Viz-Dynamics "Dynamic predicates"
        :PredStats "Predicates per module"} kw)
      (str (name kw))))

(def navigation
  [:Problems
   :Dependencies
   ["Complexity" :Indentation :AST-Nesting :Unifications :PredStats :Calls :Halstead]
   ["Visualizations" :Viz-Nesting :Viz-InterfaceSize :Viz-Dynamics]])

(defn pages []
  (doall (filter keyword? (flatten navigation))))

(defmulti page identity)

(defn app-routes []
  (secretary/set-config! :prefix "#")
  ;; --------------------
  ;; define routes here
  (defroute "/" []
    (re-frame/dispatch [:set-active-page :Problems]))

  
  (doseq [p (pages)]
    (defroute (str "/" (name p)) []
      (re-frame/dispatch [:set-active-page p])))
  
  
  ;; --------------------
  (hook-browser-navigation!))
