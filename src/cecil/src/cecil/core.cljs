(ns cecil.core
  (:require
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [sablono.core :as sab :include-macros true]
   [cecil.cki :as cki]
   [cecil.util :as util]
   [cecil.ccl-to-sql :as cts]))

(enable-console-print!)

(defn app
  []
  [:div
   [:h3 "CiCiL performs simple pattern-based transformations from CCL to Oracle SQL."]
   [:ol
    (map #(vector :li (string/upper-case %))
      (cts/about-regexes))]
   "It also uses a CKI when available."
   [:div {:key "usage"}
     [:h3 "Usage"]
     [:ol
      (map-indexed #(vector :li {:key %1} %2)
        ["Paste your CCL."
         "Click Translate."])]]])

(defn main []
  ;; conditionally start the app based on whether the #main-app-area
  ;; node is on the page
  (if-let [node (.getElementById js/document "main-app-area")]
    (.render js/ReactDOM (sab/html (app))
     node)))

(main)
