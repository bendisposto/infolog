# Infolog - ProB Sourcecode Analysis System

[![Build Status](https://travis-ci.org/bendisposto/infolog.svg?branch=master)](https://travis-ci.org/bendisposto/infolog)

# Running
To run the system you need Leiningen. The simplest way is to download [this script](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein) to a location in your path (e.g. /usr/local/bin) and make it executable.

Running the analyzer to produce a "database" can be done from the Clojure REPL

     $> lein repl

     infolog.core=> (run-prolog-analyzer "raw-data.clj") ;; run analyzer
     nil
     infolog.core=> (make-db "raw-data.clj" "database.clj") ;; indexing
     nil  

As a result you will get a file "database.clj", that can be used for the visualization.

The database can be used to visualize the module dependencies. We assume that the file "database.clj" is present.

    $> lein ring server 
