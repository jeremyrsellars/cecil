(ns cecil.core
  (:require
   #_[om.core :as om :include-macros true]
   [clojure.string :as string]
   [clojure.test :refer [testing is]]
   [clojure.pprint :as pprint]
   [sablono.core :as sab :include-macros true]
   [cecil.cki :as cki]
   [cecil.util :as util]
   [cecil.ccl-to-sql :as cts]
   [cecil.ccl-to-sql-test]
   [cecil.cki-test]
   [cecil.util-test])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]))

(enable-console-print!)

(defcard first-card
  (sab/html [:div
             [:h1 "This is your first devcard!"]]))

(defn app
  []
  [:div
   [:h3 "CiCiL performs simple pattern-based transformations from CCL to Oracle SQL."]
   [:ol
    (map #(vector [:li (string/upper-case %)])
      (cts/about-regexes))]
   "It also uses a CKI when available."
   [:h3 "Usage"]
   [:ol
    [:li "Paste your CCL."]
    [:li "Click Translate."]]])



(defn main []
  ;; conditionally start the app based on whether the #main-app-area
  ;; node is on the page
  (if-let [node (.getElementById js/document "main-app-area")]
    (.render js/ReactDOM (sab/html (app))
     node)))

(main)

(deftest show
  (is (nil? cki/cki)
    "This just shows the CKI mappings.... nothing to worry about"))
