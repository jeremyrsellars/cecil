(ns cecil.core
  (:require
   #_[om.core :as om :include-macros true]
   [clojure.test :refer [testing is]]
   [clojure.pprint :as pprint]
   [sablono.core :as sab :include-macros true]
   [cecil.cki :as cki]
   [cecil.util :as util]
   [cecil.ccl-to-sql-test]
   [cecil.cki-test]
   [cecil.util-test])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]))

(enable-console-print!)

(defcard first-card
  (sab/html [:div
             [:h1 "This is your first devcard!"]]))

(defn main []
  ;; conditionally start the app based on whether the #main-app-area
  ;; node is on the page
  (if-let [node (.getElementById js/document "main-app-area")]
    (.render js/ReactDOM (sab/html [:div "This is working"]) node)))

(main)

(deftest show
  (is (nil? cki/cki)
    "This just shows the CKI mappings.... nothing to worry about"))
