(ns cecil.reflow-test
  (:require
   #_[om.core :as om :include-macros true]
   [clojure.data :refer [diff]]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [testing is]]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [clojure.walk :as walk]
   [sablono.core :as sab :include-macros true]
   [cecil.cki :as cki]
   [cecil.util :as util]
   [cecil.ccl-to-sql :as cts]
   [cecil.reflow :as r])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cecil.test-macros :refer [insert-file-contents-string]]))


(deftest parse-ccl-breaks-query-into-chunks
   (letfn [(sql-lines [s]
            (string/split (or s "") #"\r?\n"))
           (test-reflow
              [ccl expected-lines]
              (let [[actual remaining]   (r/tokenize-and-parse (string/trim ccl))
                    actual               (-> actual r/reflow)
                    actual-sql-lines     (-> actual cts/emit-string sql-lines)
                    [missing extra same] (diff (cons "" expected-lines) actual-sql-lines)]
                (testing ccl
                  (when (seq remaining)
                    (is (empty? remaining)
                      "Extra tokens"))
                  ; (is (nil? (s/explain-data ::cts/ast-node expected))
                  ;   "Conforms to spec: expected")
                  ; (is (nil? (s/explain-data ::cts/ast-node actual))
                  ;   "Conforms to spec: actual")
                  (when missing
                    (is (nil? missing)
                     "Missing from actual"))
                  (when extra
                    (is (nil? extra)
                     "Extra in actual"))
                  (is (= (cons "" expected-lines) actual-sql-lines)
                   "The lines are the same")
                  (when (or missing extra)
                    (is (nil? actual)
                      "Show actual reflowed")))))]
                  ; (is (nil?
                  ;        (cts/emit-string actual))
                  ;   (cts/emit-string actual)))))]

      (test-reflow
        "select cat.cat_id from feline cat"
         ["select"
          "  cat.cat_id"
          "from"
          "  feline cat"])

      (test-reflow
        "select 1 from Apple a, Bee b"
        ["select"
         "  1"
         "from"
         "  Apple a,"
         "  Bee b"])

      (test-reflow
        "select cat.cat_id,ITEM_PRIMARY=cat.cat_id from feline cat"
        ["select"
         "  cat.cat_id,"
         "  ITEM_PRIMARY=cat.cat_id"
         "from"
         "  feline cat"])

     (test-reflow
        "select cat.cat_id,ITEM_PRIMARY=uar_get_code_display(cat.cat_id) from feline cat"
        ["select"
         "  cat.cat_id,"
         "  ITEM_PRIMARY=uar_get_code_display(cat.cat_id)"
         "from"
         "  feline cat"])))
