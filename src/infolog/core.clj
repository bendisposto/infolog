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

(defroutes app-routes
  (GET "/" [] (h/html [:h1 "Infolog"]
     #_[:a {:href "dependencies.html"} "Module Dependency Visualization"]
       [:a {:href "problems.html"} "linter-Results"]))
       (GET "problemlist" [] (json/write-str []))
  #_(GET "/dependencies" [] (json/write-str (mk-dep @facts)))
  #_(GET "/cycles" [] (json/write-str (cyclic-dependencies @facts)))
  #_(GET "/dependency-details/:module" [module] (json/write-str (details @facts module)))
  (route/resources "/"))

(def app (do (-> (handler/site app-routes)
             (wrap-base-url))))


(defn start-prolog-repl []
  (let [p (shell/proc "sicstus" "-i" "-l" "prolog-analyzer/analyzer.pl" :redirect-err true)
        wrt (java.io.OutputStreamWriter. (:in p))
        rdr (java.io.InputStreamReader. (:out p))]
    (.write wrt "repl.\n")
    (.flush wrt)
    (assoc p :writer wrt :reader rdr)))

(defn q [sicstus query]
  (let [w (:writer sicstus)
        r (:reader sicstus)]
    (doto w
      (.write query)
      (.write "\n")
      (.flush))
    ;;(println (shell/read-line sicstus :out))
    ;; read-result
    ;; process result
    ))


(comment

  ;; extracting the information from Prolog
  ;; make sure that the PROB_HOME env variable is set
  (run-prolog-analyzer (System/getenv "PROB_HOME") "raw-data.clj")

  ;; Create a database from the extracted information
  (make-db "raw-data.clj" "database.clj")

  ;; load the database for processing
  (def f (load-database "database.clj"))

  ;; find modules that are loaded but not using use_module
  (with-db f (run* [q] (fresh [m f d] (module m f) (noto (dependency d m)) (== q m))))

  ;; print all problems
  (doseq [x (with-db f (run* [q] (problem q)))] (println x))


  )
