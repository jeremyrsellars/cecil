(ns cecil.honey
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [cecil.standardize :as standardize]
            [cecil.util :as util]))

(defn console-log
  [& args]
  #?(:cljs (apply js/console.log args)))
(defn console-trace
  [& args]
  #?(:cljs (apply js/console.trace args)))
(defn console-warn
  [& args]
  #?(:cljs (apply js/console.warn args)))

(defn decorate-source-meta
  [x & stuff]
  (try
    (vary-meta x update ::source #(conj (or %1 []) %2) stuff)
    (catch #?(:cljs js/Error :default Exception) e
      x)))

(defn token-of-type?
 ([n kw]
  (= kw (:type n)))
 ([{:keys [type]} kw & types]
  (or (= kw type)
      (some #(= type %) types)
      false)))

(defn token-of-keyword?
 ([n kw]
  (= kw (:keyword n)))
 ([{:keys [keyword]} kw & keywords]
  (or (= kw keyword)
      (some #(= keyword %) keywords)
      false)))

(defn token-of-sub-type?
 ([{:keys [type sub-type]} type-kw sub-type-kw]
  (and (= type-kw type)
       (= sub-type-kw sub-type))))

(def ternary-operator-kws
  #{:between})

(def prevent-unwrap-keywords #{:in :not-in})

(def binary-operator-token-nodes
  (into #{} (map vector) ["*" "/" "||" "+" "-"]))

(defn binary-operator-token?
  [n]
  (or (token-of-type? n :equals :not-equals :inequality :not-compare)
      (token-of-keyword? n :in :not-in :is :is-not)
      (contains? binary-operator-token-nodes (:nodes n))))

(defn flatten-tokens
  [node]
  (->> node
    (walk/postwalk
      (fn transcode-walk
        [x]
        (if-let [nodes (:nodes x)]
          nodes
          x)))
    flatten))

(defn- parse-identifier-kw
  [{:keys [nodes sub-type] :as node}]
 ;{'parse-identifier-kw node :result
  (->> node flatten-tokens string/join keyword))

(defn- parse-operator-kw
  [{:keys [nodes sub-type] :as node}]
  (-> node flatten-tokens string/join string/lower-case (string/replace #"\s+" "-") keyword))

(defn- raw
  [{:keys [nodes] :as node}]
 ;{'parse-identifier-kw node :result
  (->> node
    (walk/prewalk #(cond (token-of-type? % :string-double) (pr-str (util/unwrap-string-double (first (:nodes %))))
                         (token-of-type? % :string-single) (pr-str (util/unwrap-string-single (first (:nodes %))))
                         :default %))
    flatten-tokens
    (string/join " ")
    (vector :raw)))

(declare
  parse-expression-nodes
  parse-selects)

(defn- paren-matching-split-with
  [pred nodes]
  (split-with pred nodes))

(defn parse-expression-nodes-until
  [opts take-while-pred & nodes]
  (let [[expr-nodes rst] (paren-matching-split-with (complement take-while-pred) nodes)]
    [(apply parse-expression-nodes opts expr-nodes)
     rst]))

(defmulti parse-ternary (fn parse-ternary_dispatch [opts left-expr op-kw right-nodes] op-kw))
  ; [ternary-expr remaining-right-nodes]

(defmethod parse-ternary :between
  [opts left-expr op-kw after-op-nodes]
  (let [[a and-b-rest] (apply parse-expression-nodes-until opts #(token-of-keyword? % :and) after-op-nodes)
        b-rest (rest and-b-rest)
        [b after-b-nodes] (apply parse-expression-nodes-until opts #(token-of-keyword? % :and :or) b-rest)]
    [[:between left-expr a b]
     after-b-nodes]))

(defmulti parse-expression-node
  (fn parse-expression-node_dispatch [opts node]
    (select-keys node [:type :sub-type :keyword])))

(defmethod parse-expression-node :default
  [opts node]
  (raw {:type :expression
        :nodes [node]}))

(defmethod parse-expression-node {:type :string-double}
  [opts {:keys [nodes] :as node}]
  (assert (== 1 (count nodes)))
  (let [encoded-string (first nodes)]
    [:inline (util/unwrap-string-double encoded-string)]))

(defmethod parse-expression-node {:type :string-single}
  [opts {:keys [nodes] :as node}]
  (assert (== 1 (count nodes)) (str (pr-str node) (string/join ", " nodes)))
  (let [encoded-string (first nodes)]
    [:inline (util/unwrap-string-single encoded-string)]))

(defmethod parse-expression-node {:type :number}
  [opts {:keys [nodes]}]
  (if-let [num-string (as-> (and (== 1 (count nodes)) (first nodes)) only-node
                        (and (string? only-node) only-node))]
    (cond (re-find #"^\d+$" num-string)      [:inline (util/parse-int num-string)]
          (re-find #"^\d+\.\d+$" num-string) [:inline (util/parse-float num-string)]

          :default (into [:raw] nodes))

    (into [:inline] nodes)))

(defmethod parse-expression-node {:type :expression, :sub-type :parenthetical}
  [opts node]
  (let [child-nodes-without-parens (-> (:nodes node) rest butlast)]
    (if (every? #(token-of-type? % :expression) child-nodes-without-parens)
      (cond-> (mapv parse-expression-node (repeat (dissoc opts :prevent-unwrap)) child-nodes-without-parens)
        (and ; unwrap when there's only one node, especially `(select...)`
             (== 1 (count child-nodes-without-parens)))
             ;(not (:prevent-unwrap opts)))
        first

        :decorate
        (decorate-source-meta `[parse-expression-node {:type :expression, :sub-type :parenthetical} ~opts ~child-nodes-without-parens]))
      (raw {:type :expression
            :nodes [node]}))))

(defmethod parse-expression-node {:type :expression, :sub-type :parenthetical-indent}
  [opts {:keys [nodes] :as node}]
  (let []
    (cond (every? #(token-of-type? % :select) nodes)
          (cond-> (mapv parse-selects (repeat opts) nodes)
            (and ; unwrap when there's only one node, especially `(select...)`
                 (== 1 (count nodes))
                 (not (:prevent-unwrap opts)))
            first

            :decorate
            (decorate-source-meta `[parse-expression-node {:type :expression, :sub-type :parenthetical-indent} 0 ~opts]))

          (or (some #(token-of-type? % :comma) nodes) ; especially `x in (1,2,3)
              (:prevent-unwrap opts))
          (->> nodes
               (partition-by #(token-of-type? % :comma))
               (remove #(token-of-type? (first %) :comma))
               (mapv #(cond-> (mapv parse-expression-node (repeat (dissoc opts :prevent-unwrap)) %)
                        (and ; unwrap
                             (not (next %)))
                             ;(not (:prevent-unwrap opts)))
                        first

                        :decorate
                        (decorate-source-meta `[parse-expression-node {:type :expression, :sub-type :parenthetical-indent} 1 ~opts]))))

          :default
          (let [results (mapv parse-expression-node (repeat (dissoc opts :prevent-unwrap)) nodes)]
            (cond-> results
              (and ; unwrap
                   (== 1 (count results))
                   (not (next nodes))
                   (not (:prevent-unwrap opts)))
              first

              :decorate
              (decorate-source-meta `[parse-expression-node {:type :expression, :sub-type :parenthetical-indent} 2 ~opts]))))))

(defmethod parse-expression-node {:type :expression}
  [opts {:keys [nodes] :as node}]
  (apply parse-expression-nodes opts nodes))

(defmethod parse-expression-node {:type :identifier, :sub-type :composite}
  [opts node]
  (parse-identifier-kw (:nodes node)))

(defmethod parse-expression-node {:type :identifier}
  [opts node]
  ;{'identifier}
  (parse-identifier-kw (:nodes node)))

(defn parse-expression-nodes-inner
  [{:keys [prevent-unwrap] :as opts} & nodes]
 (cond->
  (if (or prevent-unwrap (next nodes))
    (mapv parse-expression-node (repeat (dissoc opts :prevent-unwrap)) nodes)
    (parse-expression-node opts (first nodes)))

  :decorate
  (decorate-source-meta `[parse-expression-nodes-inner ~(or prevent-unwrap (next nodes)) ~opts])))

(defn- parse-expression-nodes-binary-eq
  [opts & nodes]
 (cond->
  (cond
    (empty? nodes)
    {'parse-expression-nodes-binary-eq :empty}

    (and (== 1 (count nodes))
         (not (:prevent-unwrap opts))) ; to-do: should I prevent unwrap?
    (do ;(console-log 'parse-expression-nodes-binary-eq opts :opt 0 nodes :result (parse-expression-node opts (first nodes)) :opts opts)
      (parse-expression-node opts (first nodes)))

    :more
    (loop [result-expr nil  ; or should this be nil and check for it below?
           nodes nodes
           prevent-unwrap false] ;(or (:prevent-unwrap opts) false)]
      (let [[left-nodes [op & right-nodes]]
            (split-with
              (complement binary-operator-token?)
              nodes)]
        (if-not op
          (let [left-expr
                (when (and (coll? left-nodes) (seq left-nodes))
                  (apply parse-expression-nodes-inner (assoc opts :prevent-unwrap prevent-unwrap) left-nodes))]
           (cond (and (vector? result-expr)
                      (vector? left-expr)
                      (= (first result-expr) (first left-expr))) ; combine [:and [:and expr-a expr-b] expr-c] -> [:and expr-a expr-b expr-c]
                 (as-> (into result-expr left-expr) x(do(console-log 'parse-expression-nodes-binary-eq :loop 'not 1 left-expr :result x (pr-str x))x))

                 left-expr
                 (as-> (conj result-expr left-expr) x(do(console-log 'parse-expression-nodes-binary-eq :loop 'not 2 left-expr :result x (pr-str x))x))

                 :default
                 (as-> result-expr x(do(console-log 'parse-expression-nodes-binary-eq :loop 'not 3 :result x (pr-str x))x))))
          (let [expr [(parse-operator-kw op)]
                left-parsed (when (seq left-nodes) (apply parse-expression-nodes-binary-eq (update opts :prevent-unwrap #(or % prevent-unwrap)) left-nodes))
                new-prevent-unwrap (or prevent-unwrap (contains? prevent-unwrap-keywords (:keyword op)))]
            (console-log 'parse-expression-nodes-binary-eq :loop 'so left-parsed {:prevent-unwrap prevent-unwrap})
            (recur
              (cond
                result-expr          (into expr (into result-expr left-parsed))
                left-parsed          (conj expr left-parsed)
                :default             expr)
              right-nodes
              new-prevent-unwrap))))))

  :decorate
  (decorate-source-meta `[parse-expression-nodes-binary-eq ~opts])))

(defn- parse-expression-nodes-binary-bin
  [opts & nodes]
 (cond->
  (cond
    (empty? nodes)
    {'parse-expression-nodes-binary-bin :empty}

    (and (== 1 (count nodes))
         (not (:prevent-unwrap opts))) ; to-do: should I prevent unwrap?
    (parse-expression-node opts (first nodes))

    :more
    (loop [result-expr nil  ; or should this be nil and check for it below?
           nodes nodes]
     (if (empty? nodes)
      (cond-> result-expr
        (:prevent-unwrap opts) vector)
      (let [[left-nodes [op & right-nodes]]
            (split-with
              (complement
                #(or (token-of-type? %    :or :and)
                     (apply token-of-keyword? % :or :and ternary-operator-kws)))  ; cheating to save time and future-proof tokenization
              nodes)]
        (if-not op
          (let [;_ (console-log 'parse-expression-nodes-binary-bin :result result-expr :loop 1 :left-expr (apply parse-expression-nodes-binary-eq left-nodes))
                left-expr (apply parse-expression-nodes-binary-eq opts left-nodes)]
            (cond->> left-expr
              result-expr (conj result-expr)))
          (if-let [op-kw (ternary-operator-kws (:keyword op))]
            (let [_ (console-log 'parse-expression-nodes-binary-bin :result result-expr 'ternary op-kw :loop 21)
                  left-expr (apply parse-expression-nodes-binary-bin opts left-nodes)
                  [ternary-expr new-right-nodes] (parse-ternary opts left-expr op-kw right-nodes)
                  [next-op & next-right-nodes] new-right-nodes]
              (if (:keyword next-op)
                (recur
                  [(parse-operator-kw next-op)
                   (cond->> ternary-expr
                     result-expr (conj result-expr))]
                  next-right-nodes)
                (recur
                  (cond->> ternary-expr
                    result-expr (conj result-expr))
                  new-right-nodes)))
           (let [additional-expr (apply parse-expression-nodes-binary-bin opts left-nodes)]
            (recur
              [(parse-operator-kw op)
               (cond->> additional-expr
                 result-expr (conj result-expr))]
              right-nodes))))))))

  :decorate
  (decorate-source-meta `[parse-expression-nodes-binary-bin ~opts])))

(defn- parse-expression-nodes
  [opts & nodes]
  (apply parse-expression-nodes-binary-bin opts nodes))

(defn- parse-comma-separated-expression-nodes
  "Handles parts of `from`, `group by` and `order by`"
  [opts & nodes]
  (let [results
        (->> nodes                                                       ; [{:type :expression}    {:type comma}  [{:type :expression}]]
          (partition-by #(token-of-type? % :field-conjunction :comma))   ; [[{:type :expression}] [{:type comma}] [{:type :expression}]]
          (remove #(token-of-type? (first %) :field-conjunction :comma)) ; [[{:type :expression}]                 [{:type :expression}]]
          (mapv #(apply parse-expression-nodes opts %)))]
    results))

(defn- parse-comma-separated-expression-nodes-by
  "Handles `group by` and `order by`"
  [opts & nodes]
  (apply parse-comma-separated-expression-nodes opts nodes))

(defn- parse-from-table-expression-nodes
  "Handles `dual`, `dual d`, `(select 1 from dual) d`, etc."
  [opts & nodes]
  (case (count nodes)
    1 (parse-expression-node opts (first nodes))
    2 [(parse-expression-node opts (first nodes)) (parse-expression-node opts (second nodes))]
      (do
        (console-warn "Unexpected node count in table expression expected table name or (select...), optionally followed by alias." 'parse-from-table-expression-nodes opts nodes)
        (mapv parse-expression-node (repeat opts) nodes))))

(defn- parse-comma-separated-expression-nodes-from
  "Handles `from`"
  [opts & nodes]
  (let [results
        (->> nodes                                                       ; [{:type :expression}    {:type comma}  [{:type :expression}]]
             (partition-by #(token-of-type? % :field-conjunction :comma))   ; [[{:type :expression}] [{:type comma}] [{:type :expression}]]
             (remove #(token-of-type? (first %) :field-conjunction :comma)) ; [[{:type :expression}]                 [{:type :expression}]]
             (mapv #(apply parse-from-table-expression-nodes opts %)))]
    results))

(defn- infer-field-alias
 ([nodes](infer-field-alias nodes (comp str first)))
 ([nodes nodes->str-fn]
  (console-log 'infer-field-alias nodes nodes->str-fn)
  (-> nodes nodes->str-fn keyword)))

(defn- parse-field-definition
  "Parses a {:type :field-definition} node, including optional 'as' alias.
  In PL/SQL, `AS` is optional in a result column definition (`select 1 AS x, 2 y`).
  Table name alias doesn't allow `AS` (`from dual ~~AS~~ x`)."
  [opts {:keys [nodes] :as fd-node}]
  (cond
    (token-of-type? fd-node :identifier)
    (as-> (parse-expression-node opts fd-node) expr
      (cond-> expr
        (:should-suggest-field-alias opts) (vector (-> nodes flatten-tokens last keyword))))

    (token-of-type? fd-node :number)
    (as-> (parse-expression-node opts fd-node) expr
      (cond-> expr
        (:should-suggest-field-alias opts) (vector (infer-field-alias nodes))))

    (token-of-type? fd-node :string-single)
    (as-> (parse-expression-node opts fd-node) expr
      (cond-> expr
        (:should-suggest-field-alias opts) (vector (infer-field-alias nodes (comp util/unwrap-string-single first)))))

    (token-of-type? fd-node :string-double)
    (as-> (parse-expression-node opts fd-node) expr
      (cond-> expr
        (:should-suggest-field-alias opts) (vector (infer-field-alias nodes (comp util/unwrap-string-double first)))))

    :default
    (if-let [[expr as alias] ; check for `field AS alias`
             (as-> (partition-by #(token-of-keyword? % :as) nodes) expr-as-alias
               (if (and (= 3 (count expr-as-alias)) (== 1 (count (last expr-as-alias) #_alias)))
                 expr-as-alias))]
      ; `expr AS alias` => [(parse expr) :alias]
      [(apply parse-expression-nodes opts expr)
       (parse-identifier-kw (first alias))]

      (if-let [[expr alias] ; check for `field alias`
               (if (and (= 2 (count nodes))
                        (as-> (nth nodes 1) alias
                              (= [:identifier nil] ((juxt :type :sub-type) alias))))
                 nodes)]
        ; `expr AS alias` => [(parse expr) :alias]
        (do
          (mapv parse-expression-nodes (repeat opts) nodes))

        (do
          (apply parse-expression-nodes opts nodes))))))

(defn- parse-select-list
  [opts {:keys [nodes]}]
  (into []
    (keep #(cond (token-of-type? % :field-conjunction :comma)              nil
                 (token-of-type? % :field-definition)                      (apply parse-field-definition opts (:nodes %))
                 :default                                                  %))
    nodes))

(defn parse-join
  [opts nodes]
  (let [[table-and-maybe-alias on expr-nodes] (partition-by #(token-of-keyword? % :on) nodes)]
    [(into [](mapv keyword (flatten-tokens table-and-maybe-alias)))
     (apply parse-expression-nodes opts expr-nodes)]))

(defmulti assoc-parsed-clause (fn assoc-parsed-clause_dispatch [q clause-kw nodes] clause-kw))

(defn assoc-parsed-join
  [opts q clause-kw nodes]
  (update q
    :join-by
    (fn [jb]
      (conj (or jb [])
        clause-kw
        (parse-join opts nodes)))))


(defmethod assoc-parsed-clause :default
  [opts q clause-kw nodes]
  (cond (re-find #"join" (name clause-kw))
        (assoc-parsed-join opts q clause-kw nodes)

        (re-find #"from" (name clause-kw)) ; comma-separated expressions
        (assoc q clause-kw
          (apply parse-comma-separated-expression-nodes-from opts nodes))
        (re-find #"order|group" (name clause-kw)) ; comma-separated expressions
        (assoc q clause-kw
          (apply parse-comma-separated-expression-nodes-by opts nodes))

        (re-find #"where|having" (name clause-kw)) ; expression
        (assoc q clause-kw
          (apply parse-expression-nodes opts nodes))

        :default
        (assoc q clause-kw
          (raw {:nodes nodes :type :raw}))))

(defn- parse-from-etc
  [opts nodes]
  (let [top-clauses
        (partition-all 2
          (partition-by
            #(or (token-of-type? %    :from :join :where :group-by :having :order-by)
                 (token-of-keyword? % :from :join :where :group-by :having :order-by))  ; cheating to save time and future-proof tokenization
            nodes))]
    (reduce
      (fn assoc-clause [q [key-nodes val-nodes]]
        (let [clause-kw
              (-> (string/join "-" (flatten-tokens key-nodes))
                  (string/replace #" " "-")
                  string/lower-case
                  keyword)]
          ;q #_
          (assoc-parsed-clause opts q clause-kw val-nodes)))
      {}
       ;:nodes nodes
       ;:top-clauses top-clauses}
      top-clauses)))

(defn- parse-selects
  [opts {select-nodes :nodes :as select-node}]
  (let [[[select distinct] select-list-etc] (split-with #(token-of-keyword? % :select :distinct) (:nodes select-node))
        select-kw (keyword (string/join "-" (map name (keep :keyword [select distinct]))))
        [select-list expression] select-list-etc
        query-set-op-kw? (fn query-set-op-kw? [n] (not (token-of-keyword? n :all :intersect :union :except)))
        [q1-from set-op_rest] (split-with             query-set-op-kw?  (:nodes expression))
        [set-op  -rest]       (split-with (complement query-set-op-kw?) set-op_rest)

        query
        (merge {select-kw (parse-select-list opts select-list)}
               (parse-from-etc opts q1-from))]
    (if (empty? set-op)
      query
      (let [set-op-kw (->> set-op flatten-tokens (string/join "-") keyword)
            query2 (parse-selects opts
                                  (if (next -rest)
                                    {:type ::additional-select :nodes -rest}
                                    (first -rest)))
            query2-same-set-type (get query2 set-op-kw)]
        (if-not query2-same-set-type
          {set-op-kw [query query2]}
          {set-op-kw (into [query] query2-same-set-type)})))))

(defn parse-ctes
  [nodes]
  (->> nodes
       (remove #(or (= :as (:keyword %))
                    (token-of-type? % :comma)))
       (partition-all 2)
       (mapv (fn [[id-node paren-node]]
              (let [[lparen selects-expr rparen] (:nodes paren-node)
                    alias-kw (-> id-node :nodes first keyword)]
                [alias-kw
                 (get-in selects-expr [:nodes 0])])))))  ; the select expression probably was already parsed by the tree-walk

(defn- parse-with
  [{with-nodes :nodes :as with-node}]
  (let [[with parenthetical-ctes select]
        with-nodes

        cte-vec (parse-ctes (:nodes parenthetical-ctes))]
    (merge
      {:with cte-vec}
       ;::parenthetical-ctes  parenthetical-ctes  ::select  select
       ;::orig with-node}
      select)))

(defn- parse-parenthetical-expression
  [& [n1 n2 :as ast-nodes]]
  (prn 'parse-parenthetical-expression  :ast-nodes ast-nodes)
  (prn 'parse-parenthetical-expression  :n1 (token-of-type? n1 :lparen) n1)
  (prn 'parse-parenthetical-expression  :n2 (token-of-sub-type? n2 :expression :parenthetical-indent) n2)
  ;(prn 'parse-parenthetical-expression  :n2_1 (token-of-type? (first (:nodes n2)) :select) (first (:nodes n2)))
  (if (and (token-of-type? n1 :lparen)
           (token-of-sub-type? n2 :expression :parenthetical-indent))
           ;(token-of-type? (first (:nodes n2)) :select))
    (do (prn 'parse-parenthetical-expression "jeremy" (first (:nodes n2)))
        (first (:nodes n2)))
    (into []
      (keep #(cond (= ::as %)              nil
                   :default %))
      ast-nodes)))

(defn- summary-string
  [dom]
  (with-out-str
    (prn dom)
    (println)
    (println "; -------------------------------------------------")
    (println)
    (pprint/pprint dom)))

(defn- clean-node
  [ast-node]
  (walk/postwalk
    (fn remove-whitespace-meta [x]
      (cond-> x
        (map? x)
        (dissoc x :indent :absolute-indent :own-line? #_:leading-whitespace)))
    ast-node))

(defn- transcode-honey
 [{:keys [type sub-type] :as ast-node} opts]
 (let [cleaned-node (clean-node ast-node)]
  (->> cleaned-node
     ; (prewalk-ancestry indention-walker-first-pass '())
     ; (walk/postwalk indention-walker-second-pass)

    (walk/prewalk
      (fn transcode-walk
        [x]
        (if-not (standardize/ast-node? x)
          x
          (let [replacement
                (cond ; table.field
                      ; (and (= type :identifier) (= sub-type :composite))
                      ; (apply keyword (keep #(cond (ident? %) (name %)) nodes))

                      ; ; identifier
                      ; (and (= type :identifier))
                      ; (do (prn :identifier x)
                      ;   (apply keyword (keep #(cond (string? %) % (ident? %) (name %)) nodes)))

                      ; ", "
                      ; (#{:field-conjunction :comma} type)
                      ; ::comma

                      ; (#{:string-double :string-single} type)
                      ; (unwrap-string (first nodes))

                      ; (= :number type)
                      ; (util/parse-number (first nodes))


                      ; "AS"
                      ; (and (= type :keyword) (#{:as :or :and :not :on :in :between :from :select :distinct  :where :having :desc :asc :with :union :all} (:keyword x)))
                      ; (keyword (namespace ::_) (name (:keyword x)))

                      ; (= type :lparen)
                      ; ::lparen

                      ; (= type :rparen)
                      ; ::rparen

                      (and (= (:type x) :select))
                      (parse-selects opts x)

                      ; (and (= type :function-invocation))
                      ; (apply parse-function-invocation nodes)

                      ; (and (= type :join))
                      ; (apply parse-join-kw nodes)

                      ; (and (= type :expression) (= sub-type :parenthetical))
                      ; (do (prn :parenthetical-expression x)(apply parse-parenthetical-expression nodes))

                      ; (= type :expression)
                      ; (do (prn :expression x)(apply parse-expression-nodes opts nodes))

                      (= (:type x) :comment)
                      (list* 'comment (->> x flatten-tokens (map (fn de-comment [s] (string/replace s #"(?m)^/\*[\u0020]?|[\u0020]?\*/$|^[\u0020]*--[\u0020]?" "")))))

                      :default
                      (do (prn x)
                        (dissoc x :indent :absolute-indent :own-line? :leading-whitespace)))]
            (prn ::x x '=> replacement)
            replacement))))
    ((cond (and (= type :expression) (= sub-type :with)) parse-with
           :default                                      (do (prn 'transcode-honey type sub-type) identity)))

    (walk/postwalk
      (fn post-transcode-walk
        [x]
        (case x
          :null        [:inline nil]

          :is          :=
          :is-not      :not=

          (cond
            (list? x) (into [] x)

            :default  x)))))))

(defn- transcode-honey-summary
  [ast-node opts]
 (let [cleaned-node (clean-node ast-node)]
  (->> (transcode-honey cleaned-node opts)
    summary-string
    (str "\r\n\r\n; ---------------- Before -----------------\r\n\r\n"
         (if false (with-out-str (pprint/pprint cleaned-node)) (pr-str cleaned-node))
         "\r\n\r\n; ----------------AFTER -----------------\r\n\r\n"))))

(defn convert
  "Parses the specified SQL string, converts to a honeySQL AST, and returns in the form of
  [ast remaining]"
  [sql options]
  (-> sql
      string/trim
      standardize/tokenize-and-parse
      (standardize/standardize options)
      (transcode-honey options)))

(defn tokenize-and-honeyize
  [sql options]
  (let [[ast remaining](convert sql options)]
    (cond-> (transcode-honey-summary ast options)
      (seq remaining)
      (str "\r\n\r\n-------- Remaining (extra or bug?) ------------\r\n"
           (with-out-str (pprint/pprint remaining))))))

(def option-keys
 [:indent
  :new-line
  :break-parenthetical-length])

#?(:cljr
    (defrecord Options [^String indent ^String new-line break-parenthetical-length] :load-ns true))
#?(:cljr
    (definterface IHoneyTranscoder
      (^String Transcode     [^String sql])))

#?(:cljs
    (defn ^:export transcode
      [sql jsObj_options]
      (let [options (set/rename
                      (js->clj jsObj_options)
                      (zipmap (map name option-keys) option-keys))]
        (tokenize-and-honeyize (str sql) options)))
   :default
    (defn transcode
      [sql options]
      (tokenize-and-honeyize (str sql) options)))

#?(:cljr
    (defrecord HoneyTranscoder [options]; :load-ns true
      IHoneyTranscoder
      (^String Transcode     [this ^String sql]
        (transcode     sql (.-options this)))))
