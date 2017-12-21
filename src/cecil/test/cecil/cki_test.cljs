(ns cecil.cki-test
  (:require
   #_[om.core :as om :include-macros true]
   [clojure.test :refer [testing is]]
   [clojure.pprint :as pprint]
   [sablono.core :as sab :include-macros true]
   [cecil.cki :as cki]
   [cecil.util :as util])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]))

(deftest cki-contents
  (is (not (empty? cki/cki)))
  (is (map? cki/cki))
  (is (every? string? (keys cki/cki)))
  (is (every? map? (vals cki/cki))))
