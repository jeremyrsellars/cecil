(ns cecil.core
  (:require
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [cljs.repl :refer [doc]]
   [sablono.core :as sab :include-macros true]
   [cecil.cki :as cki]
   [cecil.util :as util]
   [cecil.ccl-to-sql :as cts]
   [cljsjs.react.dom]))

(enable-console-print!)

(defn app
  []
  [:div
   [:h3 "CiCiL performs pattern-based transformations from CCL to Oracle SQL."]
   [:h4 "Supported CCL Functions."]
   [:ol {:key "shallow"}
    (map #(vector :li (string/upper-case %))
      (cts/about-regexes))]
   "It also uses a CKI when available."
   [:h4 "Supported deep transformations:"]
   [:ol {:key "deep"}
    (map-indexed #(vector :li {:key %1} (str %2))
      (cts/about-translations))]
   [:div {:key "usage"}
     [:h3 "Usage"]
     [:ol {:key "ol"}
      (map-indexed #(vector :li {:key %1} %2)
        ["Paste your CCL."
         "Click Translate."])]]])

(defn main []
  ;; conditionally start the app based on whether the #main-app-area
  ;; node is on the page
  (when-let [node (.getElementById js/document "main-app-area")]
    (js/ReactDOM.render (sab/html (app))
     node)))

(main)


(defn on-js-reload []
  (when-let [f js/onReload]
    (f)))
