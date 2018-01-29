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


(deftest reflow-rearranges-the-query
   (letfn [(sql-lines [s]
            (string/split (or s "") #"\r?\n"))
           (test-reflow
              [ccl expected-lines]
              (let [[actual remaining]   (r/tokenize-and-parse (string/trim ccl))
                    actual               (r/reflow actual {})
                    actual-sql-lines     (-> actual cts/emit-string sql-lines)
                    [missing extra same] (diff (drop-while string/blank? expected-lines)
                                               (drop-while string/blank? actual-sql-lines))]
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
                  (is (= expected-lines actual-sql-lines)
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
         "  feline cat"])

     (test-reflow
       "select r.* from re r where r.r > \"\" order by r.a, r.b"
       ["select"
        "  r.*"
        "from"
        "  re r"
        "where"
        "  r.r > \"\""
        "order by"
        "  r.a,"
        "  r.b"])

     (test-reflow
        "; leading comment
        select distinct ;trailing comment
        item_id,
        /*block
          comment*/
        item_id from order_cat_item_r ocir"
        ["; leading comment"
         "select distinct ;trailing comment"
         "  item_id,"
         "        /*block"
         "          comment*/"
         "  item_id"
         "from"
         "  order_cat_item_r ocir"])

     (test-reflow
        "-- leading comment
        select distinct --trailing comment
        item_id,
        /*block
          comment*/
        item_id from order_cat_item_r ocir"
        ["-- leading comment"
         "select distinct --trailing comment"
         "  item_id,"
         "        /*block"
         "          comment*/"
         "  item_id"
         "from"
         "  order_cat_item_r ocir"])

     (test-reflow
        "; leading comment
        select distinct ;trailing comment
        sir.item_id, ocs.cat_id,
        /*block
          comment*/
        ITEM_PRIMARY = uar_get_code_display(ocir.cat_id), sir.synonym_id, SYN_PRIMARY = uar_get_code_display(ocs.cat_id), ocs.mnemonic, MNEMONIC_TYPE_ID = uar_get_code_display(ocs.mnemonic_type_id), ITEM_DESC = mi.value ;trailing comment before from
        from order_cat_item_r ocir, synonym_item_r sir, order_cat_synonym ocs, med_identifier mi ;trailing comment before plan
        plan sir ;trailing comment before where
        where sir.item_id not in ( select ocir.item_id   from order_cat_item_r ocir   group by ocir.item_id   having count(ocir.cat_id) > 1 ) join ocir   where ocir.item_id = sir.item_id join ocs   where ocs.synonym_id = sir.synonym_id   and ocs.cat_id != ocir.cat_id join mi   where mi.item_id = outerjoin(ocir.item_id)   and mi.med_product_id = outerjoin(0)   and mi.primary_ind = outerjoin(1)   and mi.med_identifier_type_id = outerjoin(value(uar_get_code_by(\"MEANING\",11000,\"DESC\"))) order by sir.item_id"
        ["; leading comment"
         "select distinct ;trailing comment"
         "  sir.item_id,"
         "  ocs.cat_id,"
         "        /*block"
         "          comment*/"
         "  ITEM_PRIMARY = uar_get_code_display(ocir.cat_id),"
         "  sir.synonym_id,"
         "  SYN_PRIMARY = uar_get_code_display(ocs.cat_id),"
         "  ocs.mnemonic,"
         "  MNEMONIC_TYPE_ID = uar_get_code_display(ocs.mnemonic_type_id),"
         "  ITEM_DESC = mi.value ;trailing comment before from"
         "from"
         "  order_cat_item_r ocir,"
         "  synonym_item_r sir,"
         "  order_cat_synonym ocs,"
         "  med_identifier mi ;trailing comment before plan"
         "plan"
         "  sir ;trailing comment before where"
         "where"
         "  sir.item_id not in"
         "      (select"
         "        ocir.item_id"
         "      from"
         "        order_cat_item_r ocir"
         "      group by"
         "        ocir.item_id"
         "      having"
         "        count(ocir.cat_id) > 1 )"
         "join"
         "  ocir"
         "where"
         "  ocir.item_id = sir.item_id"
         "join"
         "  ocs"
         "where"
         "  ocs.synonym_id = sir.synonym_id"
         "  and ocs.cat_id != ocir.cat_id"
         "join"
         "  mi"
         "where"
         "  mi.item_id = outerjoin(ocir.item_id)"
         "  and mi.med_product_id = outerjoin(0)"
         "  and mi.primary_ind = outerjoin(1)"
         "  and mi.med_identifier_type_id = outerjoin(value(uar_get_code_by(\"MEANING\",11000,\"DESC\")))"
         "order by"
         "  sir.item_id"])))

