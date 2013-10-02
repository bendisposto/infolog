(ns infolog.core
  (:import infolog.PrologLexer)
  (:import java.io.ByteArrayInputStream)
  (:require [clojure.java.shell :refer [sh]]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml]
            ))

(defn transform-comments [token]
  (let [text (.getText token)
        start-line (-> token .getStartPos .getLine)
        start-column (-> token .getStartPos .getPos)
        end-line (-> token .getEndPos .getLine)
        end-column (-> token .getEndPos .getPos)]
    {:text text
     :start-line start-line
     :start-column start-column
     :end-line end-line
     :end-column end-column}))


(defn extract-comments [text]
  (let [comments (PrologLexer/lexFile text)]
    (map transform-comments comments)))

;sicstus -l prolog-analyzer/codeq_analyzer_2 --goal
                                        ;"analyze_file('./prolog-testfiles/test4.pl')."

(defn mk-sicstus-goal [filename]
  (str "analyze_file('" filename "')." ))

(defn mk-sicstus-call [filename] ["sicstus" "-l" "./prolog-analyzer/codeq_analyzer_2.pl" "--goal" (mk-sicstus-goal filename)])

(defn parse-prolog [filename]
  (let [call (mk-sicstus-call filename)
        proc (apply sh call)
        output (apply str (remove #{\newline} (:out proc)))
        parsed (-> output (.getBytes "UTF-8") ByteArrayInputStream. xml/parse zip/xml-zip)]
    parsed))

(defn analyze [filename]
  (let [prolog (parse-prolog filename)
        comments (extract-comments filename)]
    [prolog comments]))

(defn from-xml [root tag] (zip-xml/xml1-> root tag zip-xml/text read-string))

(defn nav-xml [root & path]
  (apply zip-xml/xml-> root path))

(defn module-name [root]
   (from-xml root :module))

(defn exports [root]
0.3  (for [ex (nav-xml root :exports :export)]
    {:module (from-xml ex :module) :predicate (from-xml ex :name) :arity (from-xml ex :arity)}))

(defn- split-dynamic [text]
  (into [] (.split text "/" )))

(defn dynamic [root]
  (let [module (module-name root)
        dynamics (partition 2 (zip-xml/xml-> root :dynamic_predicates :dynamics zip-xml/text split-dynamic))]
    (into #{} (map (fn [[name arity]] {:module module :predicate name :arity arity} ) dynamics))))

(defn predicates [root]
  (let [module (module-name root)]
    (for [ex (nav-xml root :predicates :predicate)]
      {:module module
       :predicate (from-xml ex :name)
       :arity (from-xml ex :arity)
       :from (apply min  (zip-xml/xml-> ex :startlines zip-xml/text read-string))
       :to (apply max  (zip-xml/xml-> ex :endlines zip-xml/text read-string))
       :dynamic (-> (zip-xml/xml-> ex :dynamic zip-xml/text) first read-string)
       :meta (-> (zip-xml/xml-> ex :meta zip-xml/text) first read-string)
       :volatile (-> (zip-xml/xml-> ex :volatile zip-xml/text) first read-string)
       :multifile (-> (zip-xml/xml-> ex :multifile zip-xml/text) first read-string)
       :meta-args (-> (zip-xml/xml-> ex :meta_args zip-xml/text) first)
       })))

