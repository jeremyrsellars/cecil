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

(deftest flatten-honey-expressions
  []
  (letfn [(test-is-nil [x]
            (is (= (nil? (h/flatten-honey-expressions x)))
              (str "Should be nil: " x)))
          (test-is-unchanged [x]
            (is (= x (h/flatten-honey-expressions x))
              (str "Shouldn't change: " (pr-str x))))]
    (test-is-nil nil)
    (test-is-nil '())

    (test-is-unchanged [1])
    (test-is-unchanged ["string"])
    (test-is-unchanged [:keyword])
    (test-is-unchanged ['symbol])
    (test-is-unchanged [[:inline 1]])
    (test-is-unchanged [[:inline 1][:inline 2][:inline 3]])

    (is (as-> [[:inline 1][:inline 2][:inline 3]] x (= x (h/flatten-honey-expressions [x]))))
    (is (as-> [[:inline 1][:inline 2]] x (= (into x x) (h/flatten-honey-expressions [x x]))))
    (as-> [[:inline 1][:inline 2]] x
      (is (= (reduce into x [x x x]) (h/flatten-honey-expressions [x [x [x] x]]))
        (pr-str (h/flatten-honey-expressions [x [x [x] x]]))))))

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
  (let [test-filter-pattern (if false #"no suggested field for" #".")]
   (letfn [(test-convert
             ;([sql expected-ast](test-convert nil sql expected-ast)
             ([why-msg sql      expected-ast]     (test-convert why-msg sql nil expected-ast))
             ([why-msg sql opts expected-ast]
              (when (re-find test-filter-pattern (str "--" why-msg "\n" sql))
               (let [[ast remaining]   (h/convert (string/trim sql) opts)
                     [missing extra same] (diff expected-ast ast)]
                (testing (str why-msg (when why-msg " |==| ") sql)
                  (when (seq remaining)
                    (is (string/blank? (apply str remaining))
                      (str "Extra tokens:" (apply pr-str remaining))))
                  ; (is nil? (with-out-str (pprint/pprint ast))))))])
                  (is (= expected-ast ast)
                    (pr-str ast))

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
                     "Extra in actual")))))))]

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
         {:select [:dual/dummy]
          :from [:dual]})

     (test-convert "dual - qualified field w/ alias"
        "select dual.dummy x from dual"
         {:select [[:dual/dummy :x]]
          :from [:dual]})

     (test-convert "dual - qualified field as alias"
        "select dual.dummy as x from dual"
         {:select [[:dual/dummy :x]]
          :from [:dual]})

     (test-convert "1 number field w/ alias"
        "select 1 as one from dual"
        {:select
           [[[:inline 1] :one]]
         :from [:dual]})

     (test-convert "dual - qualified table, qualified field no alias"
        "select d.dummy from dual d"
         {:select [:d/dummy]
          :from [[:dual :d]]})

     (test-convert "dual - qualified table, qualified field w/ alias"
        "select d.dummy x from dual d"
         {:select [[:d/dummy :x]]
          :from [[:dual :d]]})

     (test-convert "dual - qualified table, qualified field w/ as alias"
        "select d.dummy as x from dual d"
         {:select [[:d/dummy :x]]
          :from [[:dual :d]]})

     (test-convert "2 tables w/ aliases"
        "select a.dummy as a, b.dummy b from dual a, dual b"
        {:select
           [[:a/dummy :a]
            [:b/dummy :b]]
         :from [[:dual :a]
                [:dual :b]]})

     (test-convert "2 tables w/ aliases"
        "select a.dummy as a, b.dummy b from apple a, banana b"
        {:select
           [[:a/dummy :a]
            [:b/dummy :b]]
         :from [[:apple :a]
                [:banana :b]]})

     (test-convert "2 fields"
        "select cat.cat_id,cat.cat_id as ITEM_PRIMARY from feline cat"
        {:select
           [:cat/cat_id
            [:cat/cat_id :ITEM_PRIMARY]]
         :from
           [[:feline :cat]]})

     (test-convert "where & order by"
       "select r.* from re r where r.r > 9879823 order by r.a, r.b"
       {:select
          [:r/*]
        :from
          [[:re :r]]
        :where
           [:> :r/r [:inline 9879823]]
        :order-by
          [:r/a
           :r/b]})

     (test-convert "; leading comment"
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

     (test-convert "-- leading comment"
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
     (test-convert "where and"
       "   SELECT R.RCPT_ID, R.RCPT_NUM
      FROM RCPT R
      WHERE R.STATUS_ID = 111
         AND R.REASON_CD = 123123"
       {:select [:R/RCPT_ID :R/RCPT_NUM]
        :from [[:RCPT :R]]
        :where  [:and
                  [:= :R/STATUS_ID [:inline 111]]
                  [:= :R/REASON_CD [:inline 123123]]]})

     (test-convert "where = 'singlestring' literal"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.name = 'hi'"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:=
                  :R/name
                  [:inline "hi"]]})

     (test-convert "where = \"doublestring\" literal"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.name = 'hi'"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:=
                  :R/name
                  [:inline "hi"]]})

     ;; Where is
     (test-convert "where is null"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.STATUS_CD is null"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:is
                  :R/STATUS_CD
                  nil]})

     (test-convert "where is not null"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.STATUS_CD is not null"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:is-not
                  :R/STATUS_CD
                  nil]})

     ;; Where Between
     (test-convert "where between numbers"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.STATUS_CD between 111 and 222"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:between
                  :R/STATUS_CD
                  [:inline 111]
                  [:inline 222]]})

     (test-convert "where between expressions"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.STATUS_CD between (111 + 0) and (222 + 0)"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:between
                  :R/STATUS_CD
                  [:+ [:inline 111] [:inline 0]]
                  [:+ [:inline 222] [:inline 0]]]})

     (test-convert "where between expressions and another expression"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE 1 = 1 and R.STATUS_CD between 111 + 0 and 222 + 0 and 2 = 2"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where [:and
                [:and ; to-do: consider flattening and..and
                  [:= [:inline 1] [:inline 1]]
                  [:between
                    :R/STATUS_CD
                    [:+ [:inline 111] [:inline 0]]
                    [:+ [:inline 222] [:inline 0]]]]
                [:= [:inline 2] [:inline 2]]]})

     (test-convert "where between select expressions"
       "SELECT R.RCPT_ID
      FROM RCPT R
      WHERE R.STATUS_CD between (select 111 from dual) and (select 222 from dual)"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where  [:between
                  :R/STATUS_CD
                  {:select [[:inline 111]] :from [:dual]}
                  {:select [[:inline 222]] :from [:dual]}]})

     ;; Where in
     (test-convert "where in number"
       "SELECT R.RCPT_ID      FROM RCPT R      WHERE R.STATUS_CD in (1111)"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where [:in :R/STATUS_CD [[:inline 1111]]]})

     (test-convert "where in numbers"
       "SELECT R.RCPT_ID      FROM RCPT R       WHERE R.STATUS_CD in (11111, 22222)"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where [:in :R/STATUS_CD [[:inline 11111] [:inline 22222]]]})

     (test-convert "where in number"       "SELECT R.RCPT_ID      FROM RCPT R      WHERE R.STATUS_CD in ('one')"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where [:in :R/STATUS_CD [[:inline "one"]]]})

     (test-convert "where in strings"
       "SELECT R.RCPT_ID       FROM RCPT R      WHERE R.STATUS_CD in ('one', \"two\")"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where [:in :R/STATUS_CD [[:inline "one"] [:inline "two"]]]})

     (test-convert "where not in numbers"
       "SELECT R.RCPT_ID      FROM RCPT R       WHERE R.STATUS_CD not in (11111, 22222)"
       {:select [:R/RCPT_ID]
        :from [[:RCPT :R]]
        :where [:not-in :R/STATUS_CD [[:inline 11111] [:inline 22222]]]})

     ;; Select union
     (doseq [[desc should-suggest-field-alias]
             {"and suggest field alias" true
              "but do not suggest field alias" false}]
       (test-convert (str "select ... union all select, " desc)
         "SELECT 'asdfljkdfsa' as dummy FROM dual UNION ALL
          SELECT dual.dummy FROM dual       union all
          SELECT 'zqweqwe' FROM dual"
          {:should-suggest-field-alias should-suggest-field-alias}
          {:union-all
            [{:select [[[:inline "asdfljkdfsa"] :dummy]]
              :from [:dual]}
             {:select [(cond-> :dual/dummy
                         should-suggest-field-alias (vector :dummy))]
              :from [:dual]}
             {:select [(cond-> [:inline "zqweqwe"]
                         should-suggest-field-alias (vector :zqweqwe))]
              :from [:dual]}]}))

     (doseq [[desc should-suggest-field-alias]
             {"and suggest field alias" true
              "but do not suggest field alias" false}]
       (test-convert (str "select ... union select, " desc)
         "SELECT 'asdfljkdfsa' as dummy FROM dual union
          SELECT dual.dummy FROM dual       UNion
          SELECT 'zqweqwe' FROM dual"
          {:should-suggest-field-alias should-suggest-field-alias}
          {:union
            [{:select [[[:inline "asdfljkdfsa"] :dummy]]
              :from [:dual]}
             {:select [(cond-> :dual/dummy
                         should-suggest-field-alias (vector :dummy))]
              :from [:dual]}
             {:select [(cond-> [:inline "zqweqwe"]
                         should-suggest-field-alias (vector :zqweqwe))]
              :from [:dual]}]}))

     (test-convert (str "suggested field aliases are <= 30 chars, only have word characters, etc.")
       "SELECT 'a\"'
       , \"123-456-7890\"
       , 'replace space'
       , 'PrEsErVe-CaSe'
       , 'x12345678901234567890123456789TruncatesTo30Characters'
       , \"~`!@#$%^&*()_+{}|:\"\"<>?,./;'[]\\replace non-word characters\"
       , 0
       , null
       , 'select'
       FROM dual"
        {:should-suggest-field-alias true}
        {:select
         [[[:inline "a\""] :a_]
          [[:inline "123-456-7890"] :d123_456_7890]
          [[:inline "replace space"] :replace_space]
          [[:inline "PrEsErVe-CaSe"] :PrEsErVe_CaSe]
          [[:inline "x12345678901234567890123456789TruncatesTo30Characters"] :x12345678901234567890123456789] ; 29 digits
          [[:inline "~`!@#$%^&*()_+{}|:\"<>?,./;'[]\\replace non-word characters"] :u_____________________________] ; 29 underscores
          [[:inline 0] :d0]
          [[:inline nil] :n_null]
          [[:inline "select"] :k_select]] ; a SQL keyword probably can't be an alias
         :from [:dual]})

     (test-convert (str "no suggested field for *")
       "SELECT *, BR.*, (select * from dual)
       FROM dual"
        {:should-suggest-field-alias true}
        {:select
         [:*
          :BR/*
          [{:select [:*] :from [:dual]} :anonymous_expresssion]]
         :from [:dual]})

     ; parameterized queries (bind variables)
     (test-convert (str "no suggested field for *")
       "SELECT :a_bindvariable a, (select :b_bindvariable from dual) b
       FROM dual"
        {:select
         [["to-do :a_bindvariable" :a]
          [{:select ["to-do :b_bindvariable"] :from [:dual]} :b]]
         :from [:dual]})

     (comment :end))))
