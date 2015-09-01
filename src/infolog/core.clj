(ns infolog.core
  (:refer-clojure :exclude [==])
  (:require [clojure.java.shell :refer [sh]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [hiccup.middleware :refer (wrap-base-url)]
            [hiccup.core :as h]
            [clojure.data.json :as json]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:import infolog.PrologLexer java.io.ByteArrayInputStream))

(defn transform-comments [module token]
  (let [text (.getText token)
        start-line (-> token .getStartPos .getLine)
        start-column (-> token .getStartPos .getPos)
        end-line (-> token .getEndPos .getLine)
        end-column (-> token .getEndPos .getPos)]
    [module text start-line end-line]))

(defn extract-comments [[m file]]
  (when-not (.startsWith file "sicstus/")
    (let [comments (PrologLexer/lexFile file)]
      (map (partial transform-comments m) comments))))

#_(defn sicstus-call [prob-home output-file]
  ["sicstus" "-l" "./prolog-analyzer/analyzer.pl"
   "--goal"
   (str "analyze('" prob-home entry-point "','" output-file "').")])

#_(defn run-prolog-analyzer [prob-home filename]
  (apply sh (sicstus-call prob-home filename))
  filename)

(defn get-comments [modules]
  (mapcat extract-comments modules))


(def sicstus-module #{"lists" "avl" "file_systems" "codesio" "ordsets" "timeout"
                      "chr" "clpfd" "system" "between" "samsort" "random" "atts"
                      "terms" "sets" "gauge" "trees" "assoc" "xml" "process"
                      "aggregate" "tcltk" "heaps" })


(defn read-data [csv-file]
  (with-open [in-file (io/reader csv-file)]
    (doall (csv/read-csv in-file))))

(defn prepare-data [csv-file]
  (->> csv-file
      read-data
      rest
      (assoc {} :data )))


(defroutes app-routes
  (GET "/" [] (h/html [:h1 "Infolog"]
     #_[:a {:href "dependencies.html"} "Module Dependency Visualization"]
       [:a {:href "problems.html"} "linter-Results"]))
       (GET "/problemlist" [] (json/write-str (prepare-data "infolog_problems.csv")))
  #_(GET "/dependencies" [] (json/write-str (mk-dep @facts)))
  #_(GET "/cycles" [] (json/write-str (cyclic-dependencies @facts)))
  #_(GET "/dependency-details/:module" [module] (json/write-str (details @facts module)))
  (route/resources "/"))

(def app (do (-> (handler/site app-routes)
             (wrap-base-url))))
