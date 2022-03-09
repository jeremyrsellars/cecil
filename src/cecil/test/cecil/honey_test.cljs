(ns cecil.honey-test
  (:require [clojure.data :refer [diff]]
            [clojure.pprint :as pprint]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [testing is]]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [cecil.cki :as cki]
            [cecil.util :as util]
            [cecil.ccl-to-sql :as cts]
            [cecil.honey :as h])
  (:require-macros [devcards.core :as dc :refer [defcard deftest]]
                   [cecil.test-macros :refer [insert-file-contents-string]]))

(deftest honey_parse-expression-nodes-inner
  (let [number
        {:type :number, :nodes ["123"]}
        parenthetical-indent
        {:type :expression, :sub-type :parenthetical-indent, :nodes [number]}
        parenthetical
        {:type :expression, :sub-type :parenthetical,
         :nodes
         [{:type :lparen, :nodes ["("], :leading-whitespace " "}
          parenthetical-indent
          {:type :rparen, :nodes [")"]}]}

        unwrapped [:inline 123]

        test-cases {;"number"               {:node number
                    ;                        :allow-unwrap   unwrapped
                    ;                        :prevent-unwrap unwrapped
                    "parenthetical-indent" {:node parenthetical-indent
                                            :allow-unwrap   unwrapped
                                            :prevent-unwrap [unwrapped]}
                    "parenthetical"        {:node parenthetical
                                            :allow-unwrap   unwrapped
                                            :prevent-unwrap [unwrapped]}}

        wrappings {:allow-unwrap   {:prevent-unwrap false}
                   :prevent-unwrap {:prevent-unwrap true}}]
    (doseq [[node-description {:keys [node] :as test-case}] test-cases]
      (testing node-description
        (doseq [[unwrap-behavior-key opts] wrappings
                :let [expected (get test-case unwrap-behavior-key)]]
          (is (= expected
               (h/parse-expression-nodes-inner
                  opts
                  node))
           (str (name unwrap-behavior-key) " – " node-description " – " (binding [*print-meta* true](pr-str node))))

          (when (not= expected
                      (h/parse-expression-nodes-inner
                        opts
                        node))
            (is (nil? (binding [*print-meta* true] (pr-str (h/parse-expression-nodes-inner
                                                              opts
                                                              node)))))))))))

(deftest honey-converts-sql-text-to-HoneySQL-data
   (letfn [(test-convert
             ([sql expected-ast](test-convert nil sql expected-ast))
             ([why-msg sql expected-ast]
              (let [[ast remaining]   (h/convert (string/trim sql) {})
                    [missing extra same] (diff expected-ast ast)]
               ;(when (re-find #" in \('one" sql)
                (testing (str why-msg (when why-msg " |==| ") sql)
                  (when (seq remaining)
                    (is (empty? remaining)
                      "Extra tokens"))
                  ; (is nil? (with-out-str (pprint/pprint ast))))))]
                  (is (or (= expected-ast ast)))

                  ; (is (nil? ast))]
                  ; (is (nil? (s/explain-data ::cts/ast-node expected))
                  ;   "Conforms to spec: expected")
                  ; (is (nil? (s/explain-data ::cts/ast-node actual))
                  ;   "Conforms to spec: actual")

                  (when (not= expected-ast ast)
                    (is (nil? (binding [*print-meta* true] (pr-str ast)))))

                  (when missing
                    (is (nil? missing)
                     "Missing from actual"))
                  (when extra
                    (is (nil? extra)
                     "Extra in actual"))))))]

     (test-convert "dual - unqualified field no alias"
        "select dummy from dual"
        {:select [:dummy]
         :from [:dual]})

     (test-convert "dual - unqualified field w/ alias"
        "select dummy x from dual"
        {:select [[:dummy :x]]
         :from [:dual]})

     (test-convert "dual - unqualified field as alias"
        "select dummy as x from dual"
         {:select [[:dummy :x]]
          :from [:dual]})

     (test-convert "dual - qualified field no alias"
        "select dual.dummy from dual"
         {:select [:dual.dummy]
          :from [:dual]})

     (test-convert "dual - qualified field w/ alias"
        "select dual.dummy x from dual"
         {:select [[:dual.dummy :x]]
          :from [:dual]})

     (test-convert "dual - qualified field as alias"
        "select dual.dummy as x from dual"
         {:select [[:dual.dummy :x]]
          :from [:dual]})

     (test-convert "1 number field w/ alias"
        "select 1 as one from dual"
        {:select
           [[[:inline 1] :one]]
         :from [:dual]})

     (test-convert "dual - qualified table, qualified field no alias"
        "select d.dummy from dual d"
         {:select [:d.dummy]
          :from [[:dual :d]]})

     (test-convert "dual - qualified table, qualified field w/ alias"
        "select d.dummy x from dual d"
         {:select [[:d.dummy :x]]
          :from [[:dual :d]]})

     (test-convert "dual - qualified table, qualified field w/ as alias"
        "select d.dummy as x from dual d"
         {:select [[:d.dummy :x]]
          :from [[:dual :d]]})

     (test-convert "2 tables w/ aliases"
        "select a.dummy as a, b.dummy b from dual a, dual b"
        {:select
           [[:a.dummy :a]
            [:b.dummy :b]]
         :from [[:dual :a]
                [:dual :b]]})

     (test-convert "2 tables w/ aliases"
        "select a.dummy as a, b.dummy b from apple a, banana b"
        {:select
           [[:a.dummy :a]
            [:b.dummy :b]]
         :from [[:apple :a]
                [:banana :b]]})

     (test-convert "2 fields"
        "select cat.cat_id,cat.cat_id as ITEM_PRIMARY from feline cat"
        {:select
           [:cat.cat_id
            [:cat.cat_id :ITEM_PRIMARY]]
         :from
           [[:feline :cat]]})

     (test-convert "where & order by"
       "select r.* from re r where r.r > 9879823 order by r.a, r.b"
       {:select
          [:r.*]
        :from
          [[:re :r]]
        :where
           [:> :r.r [:inline 9879823]]
        :order-by
          [:r.a
           :r.b]})

     (test-convert
        "; leading comment
        select distinct ;trailing comment
        item_id,
        /*block
          comment*/
        item_id from cat_item ci"
        {;'(comment "leading comment")
         :select-distinct ;trailing comment
           [:item_id
            '(comment "block\n          comment")
            :item_id]
         :from [[:cat_item :ci]]})

     (test-convert
        "-- leading comment
        select distinct --trailing comment
        item_id,
        /*block
          comment*/
        item_id from cat_item ci"
        {;"-- leading comment"
         :select-distinct ;--trailing comment
          [:item_id
           '(comment "block\n          comment")
           :item_id]
         :from
           [[:cat_item :ci]]})

     ;; Where
     (test-convert
       "   SELECT R.RCPT_ID, R.RCPT_NUM
      FROM RCPT R
      WHERE R.STATUS_ID = 111
         AND R.REASON_CD = 123123"
       {:select [:R.RCPT_ID :R.RCPT_NUM]
        :from [[:RCPT :R]]
        :where  [:and
                  [:= :R.STATUS_ID [:inline 111]]
                  [:= :R.REASON_CD [:inline 123123]]]})

     (test-convert "where = 'singlestring' literal"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.name = 'hi'"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:=
                  :R.name
                  [:inline "hi"]]})

     (test-convert "where = \"doublestring\" literal"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.name = 'hi'"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:=
                  :R.name
                  [:inline "hi"]]})

     ;; Where Between
     (test-convert "where between numbers"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.STATUS_CD between 111 and 222"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:between
                  :R.STATUS_CD
                  [:inline 111]
                  [:inline 222]]})

     (test-convert "where between expressions"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.STATUS_CD between (111 + 0) and (222 + 0)"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:between
                  :R.STATUS_CD
                  [:+ [:inline 111] [:inline 0]]
                  [:+ [:inline 222] [:inline 0]]]})

     (test-convert "where between expressions and another expression"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE 1 = 1 and R.STATUS_CD between 111 + 0 and 222 + 0 and 2 = 2"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where [:and
                [:and ; to-do: consider flattening and..and
                  [:= [:inline 1] [:inline 1]]
                  [:between
                    :R.STATUS_CD
                    [:+ [:inline 111] [:inline 0]]
                    [:+ [:inline 222] [:inline 0]]]]
                [:= [:inline 2] [:inline 2]]]})

     (test-convert "where between select expressions"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.STATUS_CD between (select 111 from dual) and (select 222 from dual)"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:between
                  :R.STATUS_CD
                  {:select [[:inline 111]] :from [:dual]}
                  {:select [[:inline 222]] :from [:dual]}]})

     ;; Where in
     (test-convert "where in number"
       "SELECT R.RCPT_ID      FROM RCPT R      WHERE R.STATUS_CD in (1111)"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where [:in :R.STATUS_CD [[:inline 1111]]]})

     (test-convert "where in numbers"
       "SELECT R.RCPT_ID      FROM RCPT R       WHERE R.STATUS_CD in (11111, 22222)"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where [:in :R.STATUS_CD [[[:inline 11111] [:inline 22222]]]]})

     (test-convert "where in number"       "SELECT R.RCPT_ID      FROM RCPT R      WHERE R.STATUS_CD in ('one')"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where [:in :R.STATUS_CD [[:inline "one"]]]})

     (test-convert "where in strings"
       "SELECT R.RCPT_ID       FROM RCPT R      WHERE R.STATUS_CD in ('one', \"two\")"
       {:select [:R.RCPT_ID]
        :from [[:RCPT :R]]
        :where [:in :R.STATUS_CD [[[:inline "one"] [:inline "two"]]]]})

     (comment :end)))
