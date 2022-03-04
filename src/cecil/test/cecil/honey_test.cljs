(ns cecil.honey-test
  (:require #_[om.core :as om :include-macros true]
            [clojure.data :refer [diff]]
            [clojure.pprint :as pprint]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [testing is]]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [sablono.core :as sab :include-macros true]
            [cecil.cki :as cki]
            [cecil.util :as util]
            [cecil.ccl-to-sql :as cts]
            [cecil.honey :as h])
  (:require-macros [devcards.core :as dc :refer [defcard deftest]]
                   [cecil.test-macros :refer [insert-file-contents-string]]))


(deftest honey-converts-sql-text-to-HoneySQL-data
   (letfn [(sql-lines [s]
            (string/split (or s "") #"\r?\n"))
           (test-convert
             ([sql expected-ast](test-convert nil sql expected-ast))
             ([why-msg sql expected-ast]
              (let [[ast remaining]   (h/convert (string/trim sql) {})
                    ; actual               (r/reflow actual {})
                    ; actual-sql-lines     (-> actual cts/emit-string sql-lines)
                    [missing extra same] (diff expected-ast ast)]
                (testing (str why-msg (when why-msg ": ") sql)
                  (when (seq remaining)
                    (is (empty? remaining)
                      "Extra tokens"))
                  ; (is nil? (with-out-str (pprint/pprint ast))))))]
                  (is (= expected-ast ast))
                  ; (is (nil? ast)))))]
                  ; (is (nil? (s/explain-data ::cts/ast-node expected))
                  ;   "Conforms to spec: expected")
                  ; (is (nil? (s/explain-data ::cts/ast-node actual))
                  ;   "Conforms to spec: actual")
                  (when missing
                    (is (nil? missing)
                     "Missing from actual"))
                  (when extra
                    (is (nil? extra)
                     "Extra in actual"))))))]

      (test-convert "1 field"
        "select cat.cat_id from feline cat"
         {:select
           [:cat.cat_id]
          :from [:feline :cat]})

      (test-convert "2 tables"
        "select 1 from Apple a, Bee b"
        {:select
           [[[:raw "1"] :1]]
         :from [[:Apple :a]
                [:Bee :b]]})

      (test-convert "2 fields"
        "select cat.cat_id,cat.cat_id as ITEM_PRIMARY from feline cat"
        {:select
           [:cat.cat_id
            [:cat.cat_id :ITEM_PRIMARY]]
         :from
           [:feline :cat]})

     (test-convert "where & order by"
       "select r.* from re r where r.r > 1 order by r.a, r.b"
       {:select
          [:r.*]
        :from
          [:re :r]
        :where
           [:> :r.r [[:raw "1"]]] ; To-do: unwrap
        :order-by
          [:r.a
           :r.b]})

     (test-convert
        "; leading comment
        select distinct ;trailing comment
        item_id,
        /*block
          comment*/
        item_id from order_cat_item_r ocir"
        {;'(comment "leading comment")
         :select-distinct ;trailing comment"
           [:item_id
            '(comment "block\n          comment")
            :item_id]
         :from [:order_cat_item_r :ocir]})

     (test-convert
        "-- leading comment
        select distinct --trailing comment
        item_id,
        /*block
          comment*/
        item_id from order_cat_item_r ocir"
        {;"-- leading comment"
         :select-distinct ;--trailing comment
          [:item_id
           '(comment "block\n          comment")
           :item_id]
         :from
           [:order_cat_item_r :ocir]})

     (test-convert
       "   SELECT BR.BILL_NBR_DISP, BR.BILL_VRSN_NBR
      FROM BILL_REC BR
      WHERE BR.BILL_STATUS_CD = 111
         AND BR.BILL_STATUS_REASON_CD = 123123"
       {:select [:BR.BILL_NBR_DISP :BR.BILL_VRSN_NBR]
        :from [:BILL_REC :BR]
        :where  [:and
                  [:= :BR.BILL_STATUS_CD [[:raw "111"]]]
                  [:= :BR.BILL_STATUS_REASON_CD [[:raw "123123"]]]]})))
