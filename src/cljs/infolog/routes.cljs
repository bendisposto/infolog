(ns infolog.routes
    (:require-macros [secretary.core :refer [defroute]])
    (:import goog.History)
    (:require [secretary.core :as secretary]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [re-frame.core :as re-frame]))

(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(def pages ["Problems" "Dependencies" "Indentation" "AST-Nesting"])

(defmulti page identity)

(defn app-routes []
  (secretary/set-config! :prefix "#")
  ;; --------------------
  ;; define routes here
  (defroute "/" []
    (re-frame/dispatch [:set-active-page :Problems]))

  (doseq [p pages]
    (defroute (str "/" p) []
      (re-frame/dispatch [:set-active-page (keyword p)])))
  
  ;; --------------------
  (hook-browser-navigation!))
