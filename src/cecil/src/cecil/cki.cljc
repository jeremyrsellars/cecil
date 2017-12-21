(ns cecil.cki
  (:require
   [clojure.string :as string]
   #?(:clj [cecil.cki-macros :refer [cki-map]]))
  #?(:cljs (:require-macros [cecil.cki-macros :refer [cki-map]])))

(def cki (cki-map "resources/public/cki.csv"))

(defn translate-uar-get-code-by
  [ccl]
  ccl)

