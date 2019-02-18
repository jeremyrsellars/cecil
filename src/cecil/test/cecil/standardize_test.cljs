(ns cecil.standardize-test
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
   [cecil.standardize :as r]
   [devcards.core :as dc :refer [defcard deftest]])
  (:require-macros
   [cecil.test-macros :refer [insert-file-contents-string]]))

(defonce show-actual (atom false))

(swap! show-actual not)

(deftest Test_move-ws
  (is (=  {:type :identifier :sub-type :composite :leading-whitespace "hi"
           :nodes [{}]}
        (r/move :leading-whitespace
          {:type :identifier :sub-type :composite
           :nodes [{:leading-whitespace "  hi  "}]}
          [:nodes 0]
          []
          r/trim-and-concatenate))
    "WS in src only")

  (is (=  {:type :identifier :sub-type :composite :leading-whitespace "  hi  "
           :nodes [{}]}
        (r/move :leading-whitespace
          {:type :identifier :sub-type :composite :leading-whitespace "  hi  "
           :nodes [{}]}
          [:nodes 0]
          []
          r/trim-and-concatenate))
    "WS in dst only.  Nothing was moved, so nothing is trimmed.")

  (is (=  {:type :identifier :sub-type :composite :leading-whitespace "hi there"
           :nodes [{}]}
        (r/move :leading-whitespace
          {:type :identifier :sub-type :composite :leading-whitespace "  there  "
           :nodes [{:leading-whitespace "  hi  "}]}
          [:nodes 0]
          []
          r/trim-and-concatenate))
    "WS in src and dst. Combine with trim-and-concatenate")

  (is (=  {:type :identifier :sub-type :composite :leading-whitespace "hi-there"
           :nodes [{}]}
        (r/move :leading-whitespace
          {:type :identifier :sub-type :composite :leading-whitespace "there"
           :nodes [{:leading-whitespace "hi"}]}
          [:nodes 0]
          []
          (fn [src-ws dst-ws] (str src-ws "-" dst-ws))))
    "WS in src and dst. Combine with dash."))

(deftest Test_move-comments
  (is (=  {:type :select-list,
           :nodes
           [{:type :field-definition,
             :nodes
             [{:type :expression,
               :nodes
               [{:type :identifier,
                 :nodes ["item_id"],
                 :absolute-indent 1}],
               :absolute-indent 1}],
             :absolute-indent 1,
             :own-line? true,
             :leading-whitespace "\r\n    "}
            {:type :comment,
             :nodes ["/*block\n          comment*/"],
             :own-line? true,
             :absolute-indent 1,}
            {:type :field-conjunction,
             :nodes [", "],
             :own-line? true,
             :absolute-indent 1,}
            {:type :field-definition,
             :nodes
             [{:type :expression,
               :nodes
               [{:type :identifier,
                 :nodes ["item_id"],
                 :absolute-indent 1}],
               :absolute-indent 1}],
             :absolute-indent 1}],
           :absolute-indent 0}
        (r/move-comments-before-commas
          {:type :select-list,
           :nodes
           [{:type :field-definition,
             :nodes
             [{:type :expression,
               :nodes
               [{:type :identifier,
                 :nodes ["item_id"],
                 :absolute-indent 1}],
               :absolute-indent 1}],
             :absolute-indent 1,
             :own-line? true,
             :leading-whitespace "\r\n    "}
            {:type :field-conjunction,
             :nodes [", "],
             :following-comment "\n        /*block\n          comment*/",
             :own-line? true,
             :absolute-indent 1,
             :leading-whitespace "\r\n    "}
            {:type :field-definition,
             :nodes
             [{:type :expression,
               :nodes
               [{:type :identifier,
                 :nodes ["item_id"],
                 :absolute-indent 1}],
               :absolute-indent 1}],
             :absolute-indent 1}],
           :absolute-indent 0}))))

(deftest standardize-rearranges-the-query
   (letfn [(sql-lines [s]
            (string/split (or s "") #"\r?\n"))
           (test-standardize
              [ccl expected-lines]
              (let [[actual remaining]   (r/tokenize-and-parse (string/trim ccl))
                    actual               (r/standardize actual {})
                    actual-sql-lines     (->> (-> actual (cts/emit-string r/standardize-tokens) sql-lines)
                                              (drop-while string/blank?)
                                              (into []))
                    [missing extra same] (diff expected-lines actual-sql-lines)]
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

                  (when (and @show-actual (or missing extra))
                    (is (nil? actual)
                      "Show actual standardized")))))]

                  ; (is (nil?
                  ;        (cts/emit-string actual))
                  ;   (cts/emit-string actual)))))]

      (test-standardize
        "seLEct :seLEct from dual"
         ["SELECT"
          "    :seLEct"
          "FROM dual"])

      (test-standardize
        "select cat.cat_id from feline cat"
         ["SELECT"
          "    cat.cat_id"
          "FROM feline cat"])

      (test-standardize
        "select cat.cat_id,cat.cat_id as ITEM_PRIMARY from Feline Cat"
        ["SELECT"
         "    cat.cat_id"
         "    , cat.cat_id AS item_primary"
         "FROM feline cat"])

     (test-standardize
        "select cat.cat_id,uar_get_code_display(cat.cat_id) as ITEM_PRIMARY from feline cat"
        ["SELECT"
         "    cat.cat_id"
         "    , uar_get_code_display(cat.cat_id) AS item_primary"
         "FROM feline cat"])

     (test-standardize
       "select r.* from re r where r.r > \"\" order by r.a, r.b"
       ["SELECT"
        "    r.*"
        "FROM re r"
        "WHERE r.r > \"\""
        "ORDER BY r.a"
        "    , r.b"])

     (test-standardize "SELECT DISTINCT t.columna , t.columnb , t.columnc FROM atable t"
       ["SELECT DISTINCT"
        "    t.columna"
        "    , t.columnb"
        "    , t.columnc"
        "FROM atable t"])

     (test-standardize "SELECT t.tableid , ti.tableinfoid , tv.version FROM atable t INNER JOIN tableinfo ti ON t.tableid = ti.tableid INNER JOIN tableversion tv ON ti.tableinfoid = tv.tableinfoid AND tv.activeind = 1"
       ["SELECT"
        "    t.tableid"
        "    , ti.tableinfoid"
        "    , tv.version"
        "FROM atable t"
        "INNER JOIN tableinfo ti"
        "    ON t.tableid = ti.tableid"
        "INNER JOIN tableversion tv"
        "    ON ti.tableinfoid = tv.tableinfoid"
        "    AND tv.activeind = 1"])

     (test-standardize "WITH order_task_activity AS ( SELECT Row_number() OVER (PARTITION BY order_id ORDER BY updt_dt_tm DESC) as rank , order_id , task_status_cd , task_status_reason_cd FROM task_activity WHERE updt_dt_tm > :StartDate AND order_id <> 0 ) SELECT o.order_id , o.activity_type_cd AS department_code , o.hna_order_mnemonic , c.process_flg , c.item_extended_price AS price FROM orders o LEFT JOIN charge c ON o.order_id = c.order_id AND c.process_flg <> 998 AND c.offset_charge_item_id = 0 LEFT JOIN order_task_activity ota ON o.order_id = ota.order_id AND ota.rank = 1 WHERE o.oric_order_dt_tm >= :StartDate AND o.orig_order_dt_tm <= :EndDate AND Nvl(ota.task_status_reason_cd, 0) <> :TaskStatusReasonNotDoneCode"
       ["WITH order_task_activity AS ("
        "    SELECT"
        "        Row_number() OVER (PARTITION BY order_id ORDER BY updt_dt_tm DESC) AS rank"
        "        , order_id"
        "        , task_status_cd"
        "        , task_status_reason_cd"
        "    FROM task_activity"
        "    WHERE updt_dt_tm > :StartDate"
        "        AND order_id <> 0"
        ")"
        "SELECT"
        "    o.order_id"
        "    , o.activity_type_cd AS department_code"
        "    , o.hna_order_mnemonic"
        "    , c.process_flg"
        "    , c.item_extended_price AS price"
        "FROM orders o"
        "LEFT JOIN charge c"
        "    ON o.order_id = c.order_id"
        "    AND c.process_flg <> 998"
        "    AND c.offset_charge_item_id = 0"
        "LEFT JOIN order_task_activity ota"
        "    ON o.order_id = ota.order_id"
        "    AND ota.rank = 1"
        "WHERE o.oric_order_dt_tm >= :StartDate"
        "    AND o.orig_order_dt_tm <= :EndDate"
        "    AND Nvl(ota.task_status_reason_cd, 0) <> :TaskStatusReasonNotDoneCode"])


     (test-standardize
        "; leading comment
        select distinct ;trailing comment
        item_id,
        /*block
          comment*/
        item_id from order_cat_item_r ocir"
        ["; leading comment"
         "SELECT DISTINCT ;trailing comment"
         "    item_id"
         "    /*block"
         "          comment*/"
         "    , item_id"
         "FROM order_cat_item_r ocir"])

     (test-standardize
        "-- leading comment
        select distinct --trailing comment
        item_id,
        /*block
          comment*/
        item_id from order_cat_item_r ocir"
        ["-- leading comment"
         "SELECT DISTINCT --trailing comment"
         "    item_id"
         "    /*block"
         "          comment*/"
         "    , item_id"
         "FROM order_cat_item_r ocir"])))
