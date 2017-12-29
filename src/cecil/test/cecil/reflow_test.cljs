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


(let [keyword-select
        {:type :keyword
         :keyword :select
         :nodes ["select"]}

      symbol-=
       {:type :equals
        :nodes ["="]}

      symbol-lparen
       {:type :lparen
        :nodes ["("]}

      symbol-rparen
       {:type :rparen
        :nodes [")"]}

      expression-ocir_catalog_cd
       {:type :expression
        :nodes [{:type :identifier
                 :sub-type :composite
                 :nodes
                  [{:type :identifier :nodes ["ocir"]}
                   {:type :qualifying-conjunction :nodes ["."]}
                   {:type :identifier :nodes ["catalog_cd"]}]}]}
      field-definition-ocir_catalog_cd
       {:type :field-definition
        :nodes [expression-ocir_catalog_cd]
        :expression expression-ocir_catalog_cd}

      identifier-ITEM_PRIMARY
       {:type :identifier
        :nodes ["ITEM_PRIMARY"]}

      expression-ITEM_PRIMARY
       {:type :expression
        :nodes [identifier-ITEM_PRIMARY]}

      field-definition-ocir_catalog_cd-as-ITEM_PRIMARY
       {:type :field-definition
        :nodes [expression-ITEM_PRIMARY symbol-= expression-ocir_catalog_cd]
        :alias expression-ITEM_PRIMARY
        :expression expression-ocir_catalog_cd}

      identifier-UAR_GETCODE_DISPLAY
       {:type :identifier
        :nodes ["uar_get_code_display"]}

      expression-fn-call-ocir_catalog_cd
        {:type :expression,
         :nodes [{:type :function-invocation,
                  :function :uar_get_code_display,
                  :nodes [identifier-UAR_GETCODE_DISPLAY
                          {:type :expression :sub-type :parenthetical
                           :nodes [symbol-lparen expression-ocir_catalog_cd symbol-rparen]}]}]}

      field-definition-UAR_GETCODE_DISPLAY-ocir_catalog_cd_-as-ITEM_PRIMARY
       {:type :field-definition
        :nodes [expression-ITEM_PRIMARY symbol-= expression-fn-call-ocir_catalog_cd]
        :alias expression-ITEM_PRIMARY
        :expression expression-fn-call-ocir_catalog_cd}

      from-order_catalog_item_r-ocir
       {:type :expression,
        :nodes
        [{:type :keyword,
          :keyword :from,
          :nodes ["from"],
          :leading-whitespace " "}
         {:type :identifier,
          :nodes ["order_catalog_item_r"],
          :leading-whitespace " "}
         {:type :identifier,
          :nodes ["ocir"],
          :leading-whitespace " "}]}]
  (deftest parse-ccl-breaks-query-into-chunks
   (letfn [(sql-fragments [s]
            (->>
              (string/split (string/trim (or s "")) #"(?i)(?=\b(?:select|from|plan|left|right|inner|outer|join|where|group|order|go|and|on)|,)")
              (map string/trim)))
           (test-parse
              [ccl expected]
              (let [[actual remaining] (r/tokenize-and-parse ccl)
                    actual (r/reflow actual)
                    [missing extra same] (diff expected actual)]
                (testing ccl
                  ; (is (empty? remaining)
                  ;   "Extra tokens")
                  (is (nil? (s/explain-data ::cts/ast-node expected))
                    "Conforms to spec: expected")
                  (is (nil? (s/explain-data ::cts/ast-node actual))
                    "Conforms to spec: actual")
                  (is (nil? missing)
                   "Missing from actual")
                  (is (nil? extra)
                   "Extra in actual")
                  ; (is (= expected same)
                  ;  "The same parts are the same")
                  (is (= expected actual)
                   "Exact match")
                  (is (= (util/tokenize ccl)
                         (cts/emit-tokens [actual remaining])))
                  (is (nil?
                         (cts/emit-string actual))
                    (cts/emit-string actual)))))]

      (test-parse
        "select ocir.catalog_cd from order_catalog_item_r ocir"
         {:type :select
          :nodes
          [keyword-select
           {:type :select-list
            :nodes [field-definition-ocir_catalog_cd]}
           from-order_catalog_item_r-ocir]})

      (test-parse
        "select ocir.catalog_cd,ocir.catalog_cd from order_catalog_item_r ocir"
         {:type :select
          :nodes
          [keyword-select
           {:type :select-list
            :leading-whitespace " "
            :nodes [field-definition-ocir_catalog_cd
                    {:type :field-conjunction :nodes [","]}
                    field-definition-ocir_catalog_cd]}
           from-order_catalog_item_r-ocir]})

      (test-parse
        "select ocir.catalog_cd,ITEM_PRIMARY=ocir.catalog_cd from order_catalog_item_r ocir"
         {:type :select
          :nodes
          [keyword-select
           {:type :select-list
            :leading-whitespace " "
            :nodes [field-definition-ocir_catalog_cd
                    {:type :field-conjunction :nodes [","]}
                    field-definition-ocir_catalog_cd-as-ITEM_PRIMARY]}
           from-order_catalog_item_r-ocir]})

     (test-parse
        "select ocir.catalog_cd,ITEM_PRIMARY=uar_get_code_display(ocir.catalog_cd) from order_catalog_item_r ocir"
         {:type :select
          :nodes
          [keyword-select
           {:type :select-list
            :leading-whitespace " "
            :nodes [field-definition-ocir_catalog_cd
                    {:type :field-conjunction :nodes [","]}
                    field-definition-UAR_GETCODE_DISPLAY-ocir_catalog_cd_-as-ITEM_PRIMARY]}
           from-order_catalog_item_r-ocir]}))))


(defn reflow-lines
  [tokens]
  (map-indexed vector (filter (complement string/blank?) tokens)))

(deftest reflow-test
  (is (= [[0 "select"]
          [1 "1"]
          [0 "from"]
          [1 "dual"]]
         (reflow-lines (util/tokenize "select 1 from dual")))))
