(defproject cecil "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.516"]
                 [devcards "0.2.6"]
                 [sablono "0.8.5"]

                 ;; need to specify this for sablono
                 ;; when not using devcards
                 [cljsjs/react "16.8.1-0"]
                 [cljsjs/react-dom "16.8.1-0"]
                 [cljsjs/react-dom-server "16.8.1-0"]
                 #_[org.omcljs/om "1.0.0-alpha46"]
                 #_[reagent "0.6.0"]]


  :plugins [[lein-figwheel "0.5.18"]
            [lein-cljsbuild "1.1.5" :exclusions [org.clojure/clojure]]]

  :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                    "target"]

  :source-paths ["src" "test"]
  :main cecil.cli

  :cljsbuild {
              :builds [{:id "devcards"
                        :source-paths ["src" "test"]
                        :figwheel { :devcards true  ;; <- note this
                                    :on-jsload "cecil.core/on-js-reload"
                                   ;; :open-urls will pop open your application
                                   ;; in the default browser once Figwheel has
                                   ;; started and complied your application.
                                   ;; Comment this out once it no longer serves you.
                                   :open-urls ["http://localhost:3469/cards.html"]}
                        :compiler { :main       "cecil.devcards-core"
                                    :asset-path "js/compiled/devcards_out"
                                    :output-to  "resources/public/js/compiled/cecil_devcards.js"
                                    :output-dir "resources/public/js/compiled/devcards_out"
                                    :preloads [devtools.preload]
                                    :source-map-timestamp true}}
                       {:id "dev"
                        :source-paths ["src"]
                        :figwheel true
                        :compiler {:main       "cecil.core"
                                   :asset-path "js/compiled/out"
                                   :output-to  "resources/public/js/compiled/cecil.js"
                                   :output-dir "resources/public/js/compiled/out"
                                   :preloads [devtools.preload]
                                   :source-map-timestamp true}}
                       {:id "release"
                        :source-paths ["src"]
                        :compiler {:main       "cecil.core"
                                   :asset-path "js/compiled/out"
                                   :output-to  "resources/public/js/compiled/cecil.js"
                                   :optimizations :advanced}}]}

  :figwheel { :css-dirs ["resources/public/css"]
              :server-port 3469}

  :profiles {:dev {:dependencies [[binaryage/devtools "1.0.5"]
                                  [figwheel-sidecar "0.5.18"]
                                  [com.cemerick/piggieback "0.2.2"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths ["src" "dev"]
                   ;; for CIDER
                   ;; :plugins [[cider/cider-nrepl "0.12.0"]]
                   :repl-options {; for nREPL dev you really need to limit output
                                  :init (set! *print-length* 50)
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}})
