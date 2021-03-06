(defproject infolog "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"]
                 [reagent "0.5.0"]
                 [re-frame "0.4.1"]
                 [com.taoensso/encore "2.5.0"]
                 [cljs-ajax "0.3.14"]
                 [cljsjs/c3 "0.4.10-0"]
                 [cljsjs/d3 "3.5.5-3"]
                 [cljsjs/dimple "2.1.2-0"]
                 [cljsjs/bootstrap "3.3.5-0"]
                 [secretary "1.2.3"]]

  :source-paths ["src/clj"]

  :plugins [[lein-cljsbuild "1.0.6"]
            [lein-figwheel "0.3.3" :exclusions [cider/cider-nrepl]]  ]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"  ]

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs"]

                        :figwheel {:on-jsload "infolog.core/mount-root"}

                        :compiler {:main infolog.core
                                   :output-to "resources/public/js/compiled/app.js"
                                   :output-dir "resources/public/js/compiled/out"
                                   :asset-path "js/compiled/out"
                                   :libs ["src/js/deps_d3.js"
                                          "src/js/viz_complex.js"]
                                   :optimizations :none
                                   :source-map-timestamp true}}

                       {:id "min"
                        :source-paths ["src/cljs"]
                        :compiler {:main infolog.core
                                   :output-to "resources/public/js/compiled/app.js"
                                   :optimizations :advanced
                                  ;; :libs ["src/js/deps_d3.js"]
                                   :pretty-print true}}]})
