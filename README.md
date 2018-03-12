# Infolog - Prolog Sourcecode Analysis System

[![Build Status](https://travis-ci.org/bendisposto/infolog.svg?branch=master)](https://travis-ci.org/bendisposto/infolog)

# Building the Infolog System

Infolog is a set of tools that can be used to extract and visualize information about the ProB source code or other Prolog projects. The make file contains a number of build targets that can be used to trigger the analyzers. 

Here are some of the important build targets

- infolog_problems.csv Checks the prolog code for problems (missing imports, etc.). The problems are exported as a csv file.
- infolog.edn Checks the prolog code for problems and extracts information abot dependencies between modules etc. This creates a Clojure file for consumption by the visualizer
- indy.edn Runs a indentation complexity analyzer, the output is a clojure file used in the visualizer
- server Builds and runs the visualizer. It requires the Clojure build tool leiningen and a python installation.
- docs Runs InfologDoc and generates HTML documents to resources/public/docs/
- infologdoc.pdf Runs InfologDoc and generates a PDF document

For all goals it is required to set PROBPATH to the location of your ProB source repository; if you are not analyzing ProB, set PROJECTPATH to your source directory and MAINFILE to the file that should serve as the starting point.

# The Infolog System 
A version of Infolog containing the latests state of the repository is located at http://www3.hhu.de/stups/infolog

