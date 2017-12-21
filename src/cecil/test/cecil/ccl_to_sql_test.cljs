(ns cecil.ccl-to-sql-test
  (:require
   #_[om.core :as om :include-macros true]
   [clojure.test :refer [testing is]]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [sablono.core :as sab :include-macros true]
   [cecil.cki :as cki]
   [cecil.util :as util]
   [cecil.ccl-to-sql :as cts])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]))

(deftest cki-contents
  (is (not (empty? cki/cki))))


(defn make-is-translated-correctly
  [test-name test-1 ccl sql]
  (testing test-name
    (is (= (util/canonical-whitespace sql)
           (util/canonical-whitespace (test-1 ccl)))
      (str ccl "  yields-> " (test-1 ccl)))
    (is (= (util/canonical-whitespace sql)
           (util/canonical-whitespace (cts/translate-all ccl)))
      (str ccl "  yields-> " (cts/translate-all ccl)))))

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



(defcard overall-translation
  (sab/html [:code
              (string/replace
                (cts/translate-all "
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

