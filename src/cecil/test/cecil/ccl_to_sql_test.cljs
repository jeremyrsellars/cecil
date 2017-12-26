(ns cecil.ccl-to-sql-test
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
   [cecil.ccl-to-sql :as cts])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cecil.test-macros :refer [insert-file-contents-string]]))

(deftest cki-contents
  (is (not (empty? cki/cki))))

(def example-1-ccl (insert-file-contents-string "resources/example-1/detail.ccl"))
(def example-1-sql (insert-file-contents-string "resources/example-1/detail.sql"))

(def example-field-alias-ccl (insert-file-contents-string "resources/example-field-alias/detail.ccl"))
(def example-field-alias-sql (insert-file-contents-string "resources/example-field-alias/detail.sql"))

(def example-where-ccl (insert-file-contents-string "resources/example-where/detail.ccl"))
(def example-where-sql (insert-file-contents-string "resources/example-where/detail.sql"))

(def example-where-string-ccl (insert-file-contents-string "resources/example-where-string/detail.ccl"))
(def example-where-string-sql (insert-file-contents-string "resources/example-where-string/detail.sql"))

(def example-like-ccl (insert-file-contents-string "resources/example-like/detail.ccl"))
(def example-like-sql (insert-file-contents-string "resources/example-like/detail.sql"))

(def detail-1930-ccl (insert-file-contents-string "resources/1930/detail.ccl"))
(def detail-1930-sql (insert-file-contents-string "resources/1930/detail.sql"))


(defn canonical=
  [a b]
  (= (cts/canonical-whitespace-and-comments a)
     (cts/canonical-whitespace-and-comments b)))

(defn make-is-translated-correctly
  [test-name test-1 ccl sql]
  (testing test-name
    (is (= (cts/canonical-whitespace-and-comments sql)
           (cts/canonical-whitespace-and-comments (test-1 ccl)))
      (str ccl "  yields-> " (test-1 ccl)))
    (is (= (cts/canonical-whitespace-and-comments sql)
           (cts/canonical-whitespace-and-comments (cts/replace-all ccl)))
      (str ccl "  yields-> " (cts/replace-all ccl)))))

;; uar_get_code_by

(deftest uar_get_code_by-regex-parses-correctly
  (letfn [(parts [s by code-set cdf-meaning]
            (is (= [by code-set cdf-meaning]
                   (rest (re-find cts/uar_get_code_by-regex s)))))]
    (parts "uar_get_code_by(\"MEANING\",4500,\"INPATIENT\")"
                             "MEANING" "4500" "INPATIENT")
    (parts "uar_get_code_by  (  \"MEANING\"  ,  4500  ,  \"INPATIENT\"  )"
                              "MEANING"        "4500"     "INPATIENT")
    (parts "uar_get_code_by(\"X\",0,\"I\")"
                             "X" "0" "I")))



(deftest uar_get_code_by-replaces-occurrances
  (let [is-translated-correctly
        (partial make-is-translated-correctly "UAR_GET_CODE_BY" cts/translate-uar_get_code_by)]
    ;; CKI
    (is-translated-correctly
      "uar_get_code_by(\"MEANING\",4500,\"INPATIENT\")"
      "(select CODE_VALUE from CODE_VALUE cv where cv.cki = 'CKI.CODEVALUE!101131' and ACTIVE_IND = 1 /*uar_get_code_by(\"MEANING\",4500,\"INPATIENT\")*/)")

    (is-translated-correctly
      "uar_get_code_by(\"MEANING\",4500,\"LONGTERM\")"
      "(select CODE_VALUE from CODE_VALUE cv where cv.cki = 'CKI.CODEVALUE!101134' and ACTIVE_IND = 1 /*uar_get_code_by(\"MEANING\",4500,\"LONGTERM\")*/)")

    ;; No CKI
    (is-translated-correctly
      "uar_get_code_by(\"MEANING\",1234567890123456789,\"THE_LOOKUP_VALUE\")"
      "(select CODE_VALUE
        from CODE_VALUE
        where CDF_MEANING = 'THE_LOOKUP_VALUE'
          and CODE_SET = 1234567890123456789
          and ACTIVE_IND = 1
          /*uar_get_code_by(\"MEANING\",1234567890123456789,\"THE_LOOKUP_VALUE\")*/)")))

;; uar_get_code_display
(deftest uar_get_code_display-regex-parses-correctly
  (letfn [(parts [s field code]
            (is (= [field code]
                   (rest (re-find cts/uar_get_code_display-regex s)))))]
    (parts "uar_get_code_display(ocs.mnemonic_type_cd)"
                        "display" "ocs.mnemonic_type_cd")
    (parts "uar_get_code_display  ( ocs.mnemonic_type_cd )"
                        "display" " ocs.mnemonic_type_cd ")
    (parts "uar_get_code_display  ( '123456' )"
                       "display" " '123456' ")

    (parts "uar_get_code_description(ocs.mnemonic_type_cd)"
                      "description" "ocs.mnemonic_type_cd")
    (parts "uar_get_code_description  ( ocs.mnemonic_type_cd )"
                        "description" " ocs.mnemonic_type_cd ")
    (parts "uar_get_code_description  ( '123456' )"
                        "description" " '123456' ")))


(deftest uar_get_code_display-replaces-occurrances
  (let [is-translated-correctly
        (partial make-is-translated-correctly "UAR_GET_CODE_DISPLAY" cts/translate-uar_get_code_display)]
    (is-translated-correctly
      "uar_get_code_display(ocs.mnemonic_type_cd)"
      "(SELECT DISPLAY FROM CODE_VALUE WHERE CODE_VALUE = ocs.mnemonic_type_cd AND ACTIVE_IND = 1 /*uar_get_code_display(ocs.mnemonic_type_cd)*/)")

    (is-translated-correctly
      "uar_get_code_description(ocs.mnemonic_type_cd)"
      "(SELECT DESCRIPTION FROM CODE_VALUE WHERE CODE_VALUE = ocs.mnemonic_type_cd AND ACTIVE_IND = 1 /*uar_get_code_description(ocs.mnemonic_type_cd)*/)")))

;; cnvtstring
(deftest cnvtstring-regex-parses-correctly
  (letfn [(parts [s code]
            (is (= [code]
                   (rest (re-find cts/cnvtstring-regex s)))))]
    (parts "cnvtstring(ocs.mnemonic_type_cd)"
                         "ocs.mnemonic_type_cd")
    (parts "cnvtstring  ( ocs.mnemonic_type_cd )"
                        " ocs.mnemonic_type_cd ")
    (parts "cnvtstring  ( '123456' )"
                        " '123456' ")))


(deftest cnvtstring-replaces-occurrances
  (let [is-translated-correctly
        (partial make-is-translated-correctly "CNVTSTRING" cts/translate-cnvtstring)]
    (is-translated-correctly
      "CNVTSTRING(pt.qty),"
      "TO_CHAR(pt.qty) /*CNVTSTRING(pt.qty)*/,")
    (is-translated-correctly
      "TRIM(CNVTSTRING(pt.qty)),"
      "TRIM(TO_CHAR(pt.qty) /*CNVTSTRING(pt.qty)*/),")
    (is-translated-correctly
      "TRIM(CNVTSTRING(md.inv_factor_nbr, 8, 2)),"
      "TRIM(TO_CHAR(md.inv_factor_nbr) /*CNVTSTRING(md.inv_factor_nbr, 8, 2)*/),")))

;; cnvtreal
(deftest cnvtreal-regex-parses-correctly
  (letfn [(parts [s code]
            (is (= [code]
                   (rest (re-find cts/cnvtreal-regex s)))))]
    (parts "cnvtreal(ocs.mnemonic_type_cd)"
                    "ocs.mnemonic_type_cd")
    (parts "cnvtreal  ( nonreal_expr )"
                      " nonreal_expr ")
    (parts "cnvtreal  ( \"100.45\" )"
                      " \"100.45\" ")))

(deftest cnvtreal-replaces-occurrances
  (let [is-translated-correctly
        (partial make-is-translated-correctly "CNVTREAL" cts/translate-cnvtreal)]
    (is-translated-correctly
      "CNVTREAL (pt.qty),"
      "(CASE
          WHEN pt.qty NOT LIKE '%[^0-9]%'
          THEN CAST(pt.qty as bigint)
          ELSE NULL
        END /*CNVTREAL (pt.qty)*/),")))


;; cnvtupper
(deftest cnvtupper-regex-parses-correctly
  (letfn [(parts [s code]
            (is (= [code]
                   (rest (re-find cts/cnvtupper-regex s)))))]
    (parts "cnvtupper(ocs.mnemonic_type_cd)"
                     "ocs.mnemonic_type_cd")
    (parts "cnvtupper  ( nonreal_expr )"
                        " nonreal_expr ")
    (parts "cnvtupper  ( \"a.45\" )"
                       " \"a.45\" ")))

(deftest cnvtupper-replaces-occurrances
  (let [is-translated-correctly
        (partial make-is-translated-correctly "CNVTUPPER" cts/translate-cnvtupper)]
    (is-translated-correctly
      "CNVTUPPER (pt.qty),"
      "UPPER(pt.qty) /*CNVTUPPER (pt.qty)*/),")))

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
       {:type :expression
        :nodes [identifier-UAR_GETCODE_DISPLAY
                {:type :expression :sub-type :parenthetical
                 :nodes [symbol-lparen expression-ocir_catalog_cd symbol-rparen]}]}

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
              (let [[actual remaining] (cts/tokenize-and-parse ccl)
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
                         (cts/emit-tokens [actual remaining]))))))

           (test-translate-1
              [test-name ccl expected-sql]
              (let [[tokens remaining] (cts/tokenize-and-parse ccl)
                    x      (cts/translate-field-aliases tokens)
                    actual-sql (cts/emit-string [x remaining])]
               (testing test-name
                (is (= (cts/canonical-whitespace-and-comments expected-sql)
                       (cts/canonical-whitespace-and-comments actual-sql))
                  (str ccl
                       " -> "
                       expected-sql)))))

           ; In the middle of repurposing the testtranslate method.... probably want 2 diferemnt methods. 1. select. 2. from...
           (test-translate-2
              [test-name ccl expected-sql]
              (let [actual-sql (cts/translateAll ccl)]
               (testing test-name
                (is (= (cts/canonical-whitespace-and-comments expected-sql)
                       (cts/canonical-whitespace-and-comments actual-sql))
                  (str ccl
                       " -> "
                       expected-sql))
                (let [[missing extra same]
                      (diff
                        (sql-fragments (cts/canonical-whitespace-and-comments expected-sql))
                        (sql-fragments (cts/canonical-whitespace-and-comments actual-sql)))]
                  (is (nil? missing)
                   "Missing from actual")
                  (is (nil? extra)
                   "Extra in actual")))))]
                  ; (is (nil? same)
                  ;  "Show same")
                  ; (is (nil? (string/split actual-sql #"\r?\n"))
                  ;  "Show actual-sql")
                  ; (is (nil? (cts/translate-field-aliases (first (cts/tokenize-and-parse (cts/replace-all ccl)))))
                  ;  "Show actual AST")))))]

      (test-parse
        "select ocir.catalog_cd from order_catalog_item_r ocir"
         {:type :select
          :nodes
          [keyword-select
           {:type :select-list
            :leading-whitespace " "
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
           from-order_catalog_item_r-ocir]})

     (testing "Testing translation"

      (test-translate-1 "simple"
        "select ocir.catalog_cd,ITEM_PRIMARY=uar_get_code_display(ocir.catalog_cd) from order_catalog_item_r ocir"
        "select ocir.catalog_cd,uar_get_code_display(ocir.catalog_cd) AS ITEM_PRIMARY from order_catalog_item_r ocir")

      (test-translate-1 "simple-with-comments"
        "select /*multi-line
          comment*/
          ocir.catalog_cd, -- sql line comment
          ; ccl line comment
          ITEM_PRIMARY = uar_get_code_display(ocir.catalog_cd) from order_catalog_item_r ocir"
        "select /*multi-line
          comment*/
          ocir.catalog_cd, -- sql line comment
          ; ccl line comment
          uar_get_code_display(ocir.catalog_cd) AS ITEM_PRIMARY
          from order_catalog_item_r ocir")

      (test-translate-2 "example-field-alias"
        example-field-alias-ccl
        example-field-alias-sql)

      (test-translate-2 "example-where"
        example-where-ccl
        example-where-sql)

      (test-translate-2 "example-where-string"
        example-where-string-ccl
        example-where-string-sql)

      (test-translate-2 "example-like"
        example-like-ccl
        example-like-sql)

      (test-translate-2 "example-1"
        example-1-ccl
        example-1-sql)

      (test-translate-2 "1930-detail"
        detail-1930-ccl
        detail-1930-sql)))))


(defcard overall-translation
  (sab/html [:code
              (string/replace
                (cts/replace-all "
              select distinct
                sir.item_id,
                ocs.catalog_cd,
                ITEM_PRIMARY = uar_get_code_display(ocir.catalog_cd),
                sir.synonym_id,
                SYN_PRIMARY = uar_get_code_display(ocs.catalog_cd),
                ocs.mnemonic,
                MNEMONIC_TYPE_CD = uar_get_code_display(ocs.mnemonic_type_cd),
                ITEM_DESC = mi.value
              from
                order_catalog_item_r ocir,
                synonym_item_r sir,
                order_catalog_synonym ocs,
                med_identifier mi
              plan sir
              where sir.item_id not in (
                select
                  ocir.item_id
                from
                  order_catalog_item_r ocir
                group by ocir.item_id
                having count(ocir.catalog_cd) > 1
              )
              join ocir
                where ocir.item_id = sir.item_id
              join ocs
                where ocs.synonym_id = sir.synonym_id
                and ocs.catalog_cd != ocir.catalog_cd
              join mi
                where mi.item_id = outerjoin(ocir.item_id)
                and mi.med_product_id = outerjoin(0)
                and mi.primary_ind = outerjoin(1)
                and mi.med_identifier_type_cd = outerjoin(value(uar_get_code_by(\"MEANING\",11000,\"DESC\")))
              order by sir.item_id ")
                #"\n" "\r\n")]))

(deftest token-of-type-tests
  (let [token {:type :abc :nodes ["abc"]}]
    (is (cts/token-of-type? token :abc))
    (is (cts/token-of-type? token :abc :a))
    (is (cts/token-of-type? token :abc :a :b))
    (is (cts/token-of-type? token :a :abc))
    (is (cts/token-of-type? token :b :a :abc))

    (is (not (cts/token-of-type? token :a)))
    (is (not (cts/token-of-type? token :a :b)))
    (is (not (cts/token-of-type? token :a :b :c)))))
