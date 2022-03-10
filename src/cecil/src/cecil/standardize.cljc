(ns cecil.standardize
  (:refer-clojure :exclude [keyword])
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.spec.alpha :as s]
   [clojure.pprint :as pprint]
   [clojure.walk :as walk]
   [cecil.ccl-to-sql :as cts :refer [assert-ast-node
                                     assert-ast-node-and-tokens
                                     assert-ast-nodes-and-tokens
                                     assert-ast-nodes
                                     token-of-type?]]
   [cecil.util :as util]))

(def ^:dynamic *break-parenthetical-length* 600)

(def keywords
   (str "abort accept access add all alter and any array arraylen as asc assert assign at attributes audit "
        "authorization avg "
        "base_table begin between binary_integer body boolean by "
        "case cast char char_base check close cluster clusters colauth column comment commit compress connect "
        "connected constant constraint crash create current currval cursor "
        "data_base database date dba deallocate debugoff debugon decimal declare default definition delay delete "
        "desc digits dispose distinct do drop "
        "else elsif enable end entry escape except exception exception_init exchange exclusive exists exit external "
        "fast fetch file for force form from function "
        "generic goto grant group "
        "having "
        "identified if immediate in increment index indexes indicator initial initrans insert interface intersect "
        "into is "
        "key "
        "level library like limited local lock log logging long loop "
        "master maxextents maxtrans member minextents minus mislabel mode modify multiset "
        "new next no noaudit nocompress nologging noparallel not nowait null number_base "
        "object of off offline on online only open option or order out over "
        "package parallel partition pctfree pctincrease pctused pls_integer positive positiven pragma primary prior "
        "private privileges procedure public "
        "raise range raw read rebuild record ref references refresh release rename replace resource restrict return "
        "returning reverse revoke rollback row rowid rowlabel rownum rows run "
        "savepoint schema segment select separate session set share snapshot some space split sql start statement "
        "storage subtype successful synonym "
        "tabauth table tables tablespace task terminate then to trigger truncate type "
        "union unique unlimited unrecoverable unusable update use using "
        "validate values variable view views "
        "when whenever where while with work"))

(def sql-keywords
  (->>
     (string/split keywords #"\s+")
     (list* "order by" "group by" "join" "inner join" "outer join" "full outer join" "left join" "full left join" "right join" "full right join"
            "union all" "union" "except")
     (map clojure.core/keyword)
     (into #{:not-in :is-not})))

(def functions
   (str "abs acos add_months ascii asin atan atan2 average "
        "bfilename "
        "ceil chartorowid chr concat convert cos cosh count "
        "decode deref dump dup_val_on_index "
        "empty error exp "
        "false floor found "
        "glb greatest "
        "hextoraw "
        "initcap instr instrb isopen "
        "last_day least lenght lenghtb ln lower lpad ltrim lub "
        "make_ref max min mod months_between "
        "new_time next_day nextval nls_charset_decl_len nls_charset_id nls_charset_name nls_initcap nls_lower "
        "nls_sort nls_upper nlssort no_data_found notfound null nvl "
        "others "
        "power "
        "rawtohex reftohex round rowcount row_number rowidtochar rpad rtrim "
        "sign sin sinh soundex sqlcode sqlerrm sqrt stddev substr substrb sum sysdate "
        "tan tanh to_char to_date to_label to_multi_byte to_number to_single_byte translate true trunc "
        "uid upper user userenv "
        "variance vsize"))

(def types
   (str "bfile blob "
        "character clob "
        "dec "
        "float "
        "int integer "
        "mlslabel "
        "natural naturaln nchar nclob number numeric nvarchar2 "
        "real rowtype "
        "signtype smallint string "
        "varchar varchar2"))


(defn ast-node?
  [x]
  (and (map? x) (contains? x :type)))

(defn is-parenthetical-expression?
  [ast-node]
  (boolean
    (when (associative? ast-node)
      (let [{:keys [type sub-type nodes]} ast-node]
          (and (= sub-type :parenthetical)
               (= type :expression))))))

(defn valid-whitespace-or-comment?
  [s]
  (or (cts/valid-string? :whitespace s)
      (cts/valid-string? :comment s)))

(defn split-ts
  "Gets the next whitespace and comment tokens as with clojure.core/split-with."
  [tokens]
  (split-with valid-whitespace-or-comment? tokens))

(def mononymous-token->type ; tokens with a single representation (name)
  {"."  :dot
   ","  :comma
   "="  :equals
   "!=" :not-equals
   "<>" :not-compare
   "("  :lparen
   ")"  :rparen})

(defn token->type
  [s]
  (or
    (get mononymous-token->type s)
    (cond
      (re-find #"(?i)\bgroup(?:\s+by)?\b" s) :group-by
      (re-find #"(?i)\bhaving\b" s)          :having
      (re-find #"(?i)\border\s+by\b" s)      :order-by
      (re-find #"(?i)\bjoin$" s)             :join
      (re-find #"^'" s)                      :string-single
      (re-find #"^\"" s)                     :string-double
      (= "||" s)                             :concatenation
      (re-find #"[<>]+=?" s)                 :inequality
      (re-find #"^\d" s)                     :number)))

(defn flatten-tokens-keep-whitespace
  [node]
  (->> node
    (walk/postwalk
      (fn transcode-walk
        [x]
        (if-let [nodes (:nodes x)]
          (keep identity [nodes (:leading-whitespace x) (:following-comment x)])
          x)))
    flatten))

(defn token-of-keyword?
 ([n kw]
  (= kw (:keyword n)))
 ([{:keys [keyword]} kw & keywords]
  (or (= kw keyword)
      (some #(= keyword %) keywords)
      false)))

(defn canonical-keyword
  [s]
  (clojure.core/keyword (string/lower-case (string/replace s #"\s+" "-"))))

(defn string->token
  [s]
  (if (nil? s)
    {:type :terminal
     :nodes []}
    (assoc
      (if-let [tt (token->type s)]
        {:type tt}
        (let [kw (canonical-keyword s)]
          (if-let [ck (sql-keywords kw)]
            {:type :keyword
             :keyword ck}
            {:type (cond
                      (string/blank? s)         :whitespace
                      :else                     :identifier)})))
      :nodes [s])))    ; consider interning here



(defn next-token
  "Gets the next non-whitespace, non-comment token,
  combining leading whitespace and comments into the :leading-whitespace.
  trailing comments"
  [tokens]
  (let [[leading-tokens [t & rst]] (split-ts tokens)
        [following-ws-tokens rst2] (split-ts rst)
        following-ws-tokens (vec following-ws-tokens)
        last-following-ws-tokens (last following-ws-tokens)
        [following-comment-tokens rst] (if (and (> (count following-ws-tokens) 1)
                                                (cts/valid-string? :whitespace last-following-ws-tokens))
                                          [(subvec following-ws-tokens 0 (- (count following-ws-tokens) 1))
                                           (cons last-following-ws-tokens rst2)]
                                          [[]
                                           rst])]
    [(cond-> (string->token t)
        (seq leading-tokens)
        (assoc :leading-whitespace (string/join leading-tokens))

        (seq following-comment-tokens)
        (assoc :following-comment (string/join following-comment-tokens)))
     (vec rst)]))

(defn minimal-whitespace
  [s]
  (cond
    (nil? s)     nil
    (empty? s)   ""
    (string? s)  (string/replace s #"[ \t]+" " ")
    :default     s))

(defn with-minimal-whitespace
  [{ws :leading-whitespace :as ast-node}]
  (if (empty? ws)
    (dissoc ast-node :leading-whitespace)
    (assoc ast-node :leading-whitespace (minimal-whitespace ws))))

(defn remove-leading-and-trailing-whitespace-from-string
  [s]
  (let [trimmed (string/replace s #"(?m)^(?:(?![\r\n])\s)+|(?:(?![\r\n])\s)+$" "")
        x
        (when-not (string/blank? trimmed)
          trimmed)]
    x))

(defn remove-leading-whitespace-in-first-node
  "Traverses the ast-node tree and removes all leading-whitespace on the path to the first leaf."
  [{:keys [nodes leading-whitespace] :as ast-node}]
  (let [is-blank (string/blank? leading-whitespace)]
    (cond-> ast-node
      is-blank
      (dissoc :leading-whitespace)

      (not is-blank)
      (update :leading-whitespace remove-leading-and-trailing-whitespace-from-string)

      (ast-node? (first nodes))
      (update-in [:nodes 0] remove-leading-whitespace-in-first-node))))


(def top-level-keywords
 #{:select
   :with
   :from
   :where
   :plan
   :join
   :order
   :order-by
   :group-by
   :having})

(defmulti relative-indent
  (fn [ancestor-nodes {:keys [type] :as ast-node}]
    type))

(defmethod relative-indent :default
  [_ _]
  0)

(defmulti keyword-own-line?
  (fn [{:keys [keyword] :as ast-node}]
    (or keyword :default)))

(defmethod keyword-own-line? :is-not         [_] false)
(defmethod keyword-own-line? :not-in         [_] false)
(defmethod keyword-own-line? :in             [_] false)
(defmethod keyword-own-line? :and            [_] true)
(defmethod keyword-own-line? :or             [_] true)
(defmethod keyword-own-line? :on             [_] true)

(defmethod keyword-own-line? :default
  [{:keys [keyword] :as ast-node}]
  (contains? top-level-keywords keyword))


(defmulti node-own-line?
  (fn [ancestor-nodes {:keys [type] :as ast-node}]
    type))

(defmethod node-own-line? :default
  [_ {:keys [type keyword] :as ast-node}]
  false)


(defmethod relative-indent :comment
  [_ _]
  1)

(defmethod node-own-line? :expression
  [ancestor-nodes {:keys [type keyword sub-type nodes] :as ast-node}]
  (case sub-type
    :parenthetical
    (let [emited (cts/emit-string (assoc ast-node :type :measure-length))]
      (or (<= *break-parenthetical-length* (count emited))
          (boolean (re-find #"\\n" (string/trim emited)))))

    :parenthetical-indent
    (let [emited (cts/emit-string (assoc ast-node :type :measure-length))]
      (or (<= *break-parenthetical-length* (count emited))
          (boolean (re-find #"\\n" (string/trim emited)))))

    false))

(defmethod relative-indent :expression
  [ancestor-nodes {:keys [sub-type] :as ast-node}]
  (or
    (when-let [{ancester-type :type :as ancestor} (first (filter ast-node? ancestor-nodes))]
      (cond
        (and (= :parenthetical-indent sub-type) (node-own-line? (cons ast-node ancestor-nodes) (-> ast-node :nodes first)))
        1

        (or (node-own-line? ancestor-nodes ast-node)
            (and (= :expression ancester-type)
                 (node-own-line? ancestor-nodes ancestor)))
        1

        (and (not= ancester-type :function-invocation)
             (is-parenthetical-expression? ast-node))
        0))
    0))

(defmethod node-own-line? :lparen
  [_ _]
  false)

(defmethod relative-indent :field-conjunction
  [_ _]
  1)

(defmethod relative-indent :field-definition
  [_ _]
  1)

(defmethod node-own-line? :field-definition
  [_ {:keys [type keyword] :as ast-node}]
  true)

(defmethod node-own-line? :join
  [ancestor-nodes ast-node]
  true)

(defmethod node-own-line? :group-by
  [ancestor-nodes ast-node]
  true)

(defmethod node-own-line? :order-by
  [ancestor-nodes ast-node]
  (if-let [ancestor-kw (some #(when (ast-node? %) (-> % :nodes first :keyword)) ancestor-nodes)]
    (= ancestor-kw :from)
    false))

(defmethod node-own-line? :keyword
  [ancestor-nodes ast-node]
  (keyword-own-line? ast-node))

(defmethod relative-indent :keyword
  [ancestor-nodes {:keys [keyword] :as ast-node}]
  (cond
    (contains? top-level-keywords keyword)
    0

    (#{:on :in :not-in} keyword)
    0

    :default
    (if-let [{ancester-type :type :as ancestor} (first (filter ast-node? ancestor-nodes))]
      (if (and (= :select ancester-type) (= :distinct keyword))
        nil
        0)
      nil)))

(defmethod node-own-line? :select ; = :type
  [_ _]
  true)

(defmethod node-own-line? :whitespace
  [_ {:keys [type keyword] :as ast-node}]
  false)


(declare parse-select)
(declare parse-expression)


(defn trim-and-concatenate
  [^String src-ws ^String dst-ws]
  (let [trimmed-src-ws (when (seq src-ws) (string/trim src-ws))
        trimmed-dst-ws (when (seq dst-ws) (string/trim dst-ws))]
    (cond
      (and (seq trimmed-src-ws) (seq trimmed-dst-ws))
      (str trimmed-src-ws
           " "
           trimmed-dst-ws)

      (or (seq trimmed-src-ws) (seq trimmed-dst-ws))
      (str trimmed-src-ws trimmed-dst-ws)

      (or src-ws dst-ws)
      (minimal-whitespace (str src-ws dst-ws)))))


(defn move
  [key expr src-path dst-path combine-whitespace]
  (assert (vector? src-path))
  (assert (vector? dst-path))
  (let [src-ws-path (conj src-path key)
        src-ws      (get-in expr src-ws-path)
        dst-ws-path (conj dst-path key)
        dst-ws      (get-in expr dst-ws-path)]
    (cond-> expr
      (seq src-ws) (->
                    (update-in src-path dissoc key)
                    (assoc-in dst-ws-path (combine-whitespace src-ws dst-ws))))))

;; parse-* functions generally take `[token]` and return `[the-parsed-thing remaining-tokens]`

(defn parse-identifier
  ([tokens] ; parse the first identifier, then recurse
   (let [[id remaining] (next-token tokens)]
      (parse-identifier [id] remaining)))
  ([expression-definitions tokens]
   (let [[maybe-dot remaining] (next-token tokens)]
      (if (token-of-type? maybe-dot :dot)
        (let [[id remaining2] (next-token remaining)]
          (parse-identifier
            (conj expression-definitions
              (assoc maybe-dot :type :qualifying-conjunction)
              id)
            remaining2))
        [(if (= 1 (count expression-definitions))
            (first expression-definitions)
            (move :leading-whitespace
              {:type :identifier :sub-type :composite
               :nodes (vec expression-definitions)}
              [:nodes 0]
              []
              trim-and-concatenate))
         tokens]))))

(defn parse-parenthetical
  "Returns [expression remaining-tokens]"
  [tokens]
  (let [[ft tokens] (next-token tokens)]
    (loop [parts []
           tokens tokens]
      (let [[nt remaining] (next-token tokens)]
        (when (token-of-type? nt :terminal)
          (pprint/pprint ["WARNING: unterminated parenthetical-expression:"
                          parts]))

        (cond
          (token-of-type? nt :rparen :terminal)
          [{:type :expression :sub-type :parenthetical
            :nodes
            [ft
             {:type :expression :sub-type :parenthetical-indent
              :nodes parts}
             nt]}
           remaining]

          (token-of-type? nt :comma)
          (recur
            (conj parts nt)
            remaining)

          (and (token-of-type? nt :keyword)
               (-> nt :keyword (= :select)))
          (let [[sel tokens] (parse-select tokens)]
            (recur
              (conj parts (remove-leading-whitespace-in-first-node sel))
              tokens))

          ; non-terminal
          :else
          (let [[expr remaining] (parse-expression tokens #(token-of-type? % :rparen :comma))]
              (recur (conj parts expr) remaining)))))))

(defn parse-expression
  [tokens & terminal-fns]
  (loop [parts []
         tokens tokens]
    (let [[nt remaining] (next-token tokens)]
      (cond
        (nil? nt)
        [{:type :expression
          :nodes parts}
         remaining]

        ;terminal or empty
        (or (token-of-type? nt :terminal)
            (some #(% nt) terminal-fns))
        [(if (== 1 (count parts))
           (first parts)
           {:type :expression
            :nodes (assert-ast-nodes parts)})
         tokens] ; don't eat terminal

        ; non-terminal
        :else
        (let [[expr remaining]
              (cond
                ; parenthetical?
                (token-of-type? nt :lparen)
                (parse-parenthetical tokens)

                ; function-invocation
                (and (token-of-type? nt :identifier)
                     (let [[nt _] (next-token remaining)]
                        (token-of-type? nt :lparen)))
                (let [[parenthetical remaining] (parse-parenthetical remaining)]
                  [{:type :function-invocation
                    :function (canonical-keyword (first (get nt :nodes)))
                    :nodes [nt parenthetical]}
                   remaining])

                :else
                (parse-identifier tokens))]
          (recur
            (conj parts (assert-ast-node expr))
            remaining))))))

(defn parse-field-definition
 ([tokens]
  (let [[expression rst] (parse-expression tokens #(token-of-type? % :equals :comma)
                                                  #(-> % :keyword (= :from)))]
   (assert-ast-node-and-tokens
    (parse-field-definition
      [{:type :field-definition
        :nodes [(assert-ast-node expression)]}]
      rst))))

 ([field-definitions tokens]
  (let [[nt remaining] (next-token tokens)]
   (assert-ast-node-and-tokens
      (if (token-of-type? nt :equals)
        (let [[part2 remaining2] (parse-field-definition remaining)
              {:keys [nodes]} (first field-definitions)]
          [{:type :field-definition
            :nodes (assert-ast-nodes (into (conj nodes nt) (get part2 :nodes)))}
           remaining2])
        [(first (assert-ast-nodes field-definitions)) tokens])))))

(defn parse-select-list
 ([tokens] ; parse the first field definition, then recurse
  (let [[fd remaining] (parse-field-definition tokens)]
    (parse-select-list [fd] remaining)))
 ([field-definitions tokens]
  (let [[nt remaining] (next-token tokens)]
    (assert-ast-nodes-and-tokens
      (if (token-of-type? nt :comma)
        (let [[fd remaining2] (parse-field-definition remaining)]
          (parse-select-list
            (conj field-definitions
              (assoc nt :type :field-conjunction)
              fd)
            remaining2))
        [field-definitions tokens])))))

(defn insert-newline-before-commas-etc
  [{:keys [nodes] :as ast-node}]
  (if (> 2 (count nodes))
    ast-node
    (let [{:keys [type sub-type]} ast-node
          lazy-should-break-on-commas
          (delay (or (not= :expression type)
                     (and (not= :parenthetical sub-type)
                          (not= :parenthetical-indent sub-type))
                     (<= *break-parenthetical-length* (count (cts/emit-string ast-node)))))]
      (assoc ast-node
        :nodes
        (into []
          (cons (first nodes)
            (map
              (fn [{prev-indent :absolute-indent, prev-keyword :keyword :as prev-node} {:keys [absolute-indent] :as node}]
                (let [{:keys [own-line?] :as new-node}
                      (cond-> node
                        (and (token-of-type? node :comma :field-conjunction) @lazy-should-break-on-commas)
                        (assoc :own-line? true
                               :nodes [", "])

                        (and (token-of-type? prev-node :comma :field-conjunction) @lazy-should-break-on-commas)
                        (assoc :leading-whitespace (string/trim (get node :leading-whitespace ""))
                               :own-line? false)

                        (and (some? prev-indent)
                             (not= absolute-indent prev-indent)
                             (not= prev-keyword :select))
                        (assoc :own-line? true))]
                  (cond-> new-node
                    own-line?
                    remove-leading-whitespace-in-first-node)))
              nodes
              (rest nodes))))))))

(defn move-comments-before-commas
  [{:keys [nodes] :as ast-node}]
  (if (> 2 (count nodes))
    ast-node
    (assoc ast-node
      :nodes
      (reduce into
        []
        (map
          (fn move-ws-after-comma [{:keys [leading-whitespace following-comment] :as node}]
            (if-not (and (token-of-type? node :comma :field-conjunction)
                         (pos? (+ (count leading-whitespace) (count following-comment))))
              [node]
              [(-> node
                   (dissoc :following-comment :leading-whitespace)
                   (assoc :nodes [(trim-and-concatenate leading-whitespace following-comment)]
                          :type :comment))
               (dissoc node :following-comment :leading-whitespace)]))
          nodes)))))

(defn parse-from-etc-for-set-ops
  "Parsing for the possibility of union*/except in {:nodes[from x ... union ...]}
  Searches the tokens (counting parenthetical depth) for a :select token;
  if found, calls parse-select (indirect recursion),
  returning (let [[select-node remaining-tokens](parse-select remaining-tokens)]
               [[... select-node] remaining-tokens])
  Returns value equivalent to [from-etc nil] when there aren't any set operations (of additional select expressions)."
  [from-etc-node]
  ;(prn 'parse-from-etc-for-set-ops from-etc-node)

  (loop [depth 0
         ts (seq (:nodes from-etc-node))
         new-from-etc []]
    ;(prn 'parse-from-etc-for-set-ops :loop depth (first ts))
    (if-not ts
      [(assoc from-etc-node :nodes new-from-etc) ; node
       nil]                                      ; remaining tokens
      (let [t (first ts)]
        (cond
          (and (zero? depth) (token-of-keyword? t :select))
          (let [[select-node remaining-tokens] (parse-select (flatten-tokens-keep-whitespace ts))]
            [(assoc from-etc-node :nodes (conj new-from-etc select-node)) ; node
             remaining-tokens])                                           ; remaining tokens

          (token-of-type? t :lparen)     (recur (inc depth) (next ts) (conj new-from-etc t))
          (token-of-type? t :rparen)     (recur (dec depth) (next ts) (conj new-from-etc t))
          :default                       (recur depth (next ts) (conj new-from-etc t)))))))

(defn parse-select
  [tokens]
  ;(prn 'parse-select ::tokens tokens)
  (let [[kw tokens] (next-token tokens)
        [distinct tokens] (let [[{:keys [type keyword] :as d} toks] (next-token tokens)]
                            (if (and (= :keyword type) (= keyword :distinct))
                              [(assoc d :leading-whitespace " ") toks]
                              [nil tokens]))
        [select-list-parsed tokens] (parse-select-list tokens)
        [next-expression remaining] (parse-expression tokens #(token-of-type? % :rparen))
        from-etc (assoc next-expression
                    :nodes
                    (loop [nodes (get next-expression :nodes)
                           prev-indent 0
                           prev-top-level? false
                           prev-same-line? false
                           new-nodes []]
                      (if-not (seq nodes)
                        new-nodes
                        (let [[{:keys [keyword type] :as node} & remaining-nodes] nodes
                              is-top-level? (contains? top-level-keywords (or keyword type))
                              is-same-line? (and (not is-top-level?)
                                                 prev-same-line?
                                                 (not (or (token-of-type? node :comma :field-conjunction)
                                                          (and (token-of-type? node :keyword)
                                                               (keyword-own-line? node)))))
                              indent (cond is-top-level?             0
                                           is-same-line?             prev-indent
                                           :default                  1)
                              new-node (assoc node :indent indent)]
                          (recur
                            remaining-nodes
                            indent
                            is-top-level?
                            (or is-top-level? is-same-line?)
                            (conj new-nodes new-node))))))
        ;_ (prn 'parse-select ::from-etc from-etc)
        ;_ (clojure.pprint/pprint ['parse-select ::from-etc from-etc])
        [from-etc2 more-remaining] (parse-from-etc-for-set-ops (assert-ast-node from-etc))
        ;_ (prn '[from-etc2 more-remaining] [from-etc2 more-remaining])
        select-list
        {:type :select-list
         :nodes (assert-ast-nodes select-list-parsed)}]
   (assert-ast-node-and-tokens
     [{:type :select
       :nodes (into []
                (filter some?
                 [(remove-leading-whitespace-in-first-node kw)
                  distinct
                  select-list
                  (assert-ast-node from-etc2)]))} ; maybe parse additional select expressions here
      (cond->> remaining
        (seq more-remaining) (concat more-remaining))])))

(defn parse-with
  [tokens]
  (let [[nt tokens] (next-token tokens)]
    (loop [parts [nt]
           tokens tokens]
      (let [[nt remaining] (next-token tokens)]
        (cond
          (token-of-type? nt :terminal)
          [{:type :expression, :sub-type :with
            :nodes (conj parts nt)}
           remaining]

          (and (token-of-type? nt :keyword)
               (-> nt :keyword (= :select)))
          (let [[sel tokens] (parse-select tokens)]
            (recur
              (conj parts sel)
              tokens))

          (token-of-type? nt :lparen)
          (let [[expr remaining] (parse-parenthetical tokens)]
            (recur (conj parts expr) remaining))

          ; non-terminal
          :else
          (let [[expr remaining] (parse-expression tokens #(-> % :keyword (= :select)))]
            (recur (conj parts expr) remaining)))))))

(defn parse-with-or-select
  [tokens]
  (let [[kw with-tokens] (next-token tokens)]
    (case (:keyword kw)
      :with (parse-with tokens)
      (parse-select tokens))))

(defn tokenize-and-parse
  [sql]
  (let [tokens (util/tokenize sql)]
   (assert-ast-node-and-tokens
    (parse-with-or-select tokens))))

(defn prewalk-ancestry
  "Like clojure.walk/prewalk, but includes hiarchical ancestry."
  [f ancestry form]
  (let [new-form (f ancestry form)]
    (walk/walk (partial prewalk-ancestry f (cons new-form ancestry)) identity new-form)))

(defn node-indent-first-pass
  [ancestor-nodes {current-indent :indent :as ast-node}]
  (let [{:keys [indent absolute-indent own-line?]}
        (->> ancestor-nodes
             (filter #(and (ast-node? %) (contains? % :absolute-indent)))
             first)]
    (-> ast-node
        (assoc :absolute-indent
          (util/nil-safe-+
            absolute-indent
            current-indent
            (relative-indent ancestor-nodes ast-node)))
        (update :own-line? #(or % (node-own-line? ancestor-nodes ast-node)))
        insert-newline-before-commas-etc
        move-comments-before-commas)))

(defn indention-walker-first-pass
  [ancestor-nodes x]
  (if (ast-node? x)
    (node-indent-first-pass ancestor-nodes x)
    x))

(defn node-indent-second-pass
  [ast-node]
  (insert-newline-before-commas-etc ast-node))

(defn indention-walker-second-pass
  [x]
  (if (ast-node? x)
    (node-indent-second-pass x)
    x))

(defn ws-walker
  [indent-ws nl-ws]
  (fn ws-walk
    [x]
    (if-not (ast-node? x)
      x
      (let [{:keys [absolute-indent indent own-line? type keyword leading-whitespace]} x]
        (cond-> x
          (not (some? indent))
          (dissoc :indent)

          (not own-line?)
          (dissoc :own-line?)

          true
          with-minimal-whitespace

          (= type :field-definition)
          remove-leading-whitespace-in-first-node

          (and own-line? (some? absolute-indent))
          (assoc :leading-whitespace (string/replace (or leading-whitespace "") #"^\s*" (string/join (cons nl-ws (repeat absolute-indent indent-ws))))))))))

(defn standardize
  [ast-node options]
  (binding [*break-parenthetical-length*
            (or (get options :break-parenthetical-length) *break-parenthetical-length*)]
    (->> ast-node
         (prewalk-ancestry indention-walker-first-pass '())
         (walk/postwalk indention-walker-second-pass)
         (walk/prewalk
          (ws-walker
            (or (get options :indent)   "    ")
            (or (get options :new-line) "\r\n"))))))


(let [key-fn string/lower-case
      title-case (fn title-case [s] (when (seq s) (str (string/upper-case (str (first s))) (string/lower-case (apply str (rest s))))))
      keywords (->> (string/split keywords #"\s+") (list* "order by" "group by" "join" "inner join" "outer join" "full outer join" "left join" "full left join" "right join" "full right join") (map string/upper-case) (into #{}))
      types (->> (string/split types #"\s+") (map string/upper-case) (into #{}))
      functions (->> (string/split functions #"\s+") (map title-case) (into #{}))
      replacements (->> (concat keywords types functions)
                        (map #(vector (key-fn %) %))
                        (into {}))]
  (def standardize-tokens
    (partial map (fn standardize-token [t]
                    (or (get replacements (key-fn t))
                        (when (re-find #"^\w" t)
                          (string/lower-case t))
                        t)))))

(defn tokenize-and-standardize
  [sql options]
  (-> sql
      string/trim
      tokenize-and-parse
      (standardize options)
      (cts/emit-string standardize-tokens)))

(defn standardize-case
  [sql options]
  (->> sql
       string/trim
       util/tokenize
       standardize-tokens
       (string/join "")
       cts/remove-empty-lines))

(def option-keys
 [:indent
  :new-line
  :break-parenthetical-length])

#?(:cljr
    (defrecord Options [^String indent ^String new-line break-parenthetical-length] :load-ns true))
#?(:cljr
    (definterface ISqlNormalizer
      (^String Normalize     [^String sql])
      (^String NormalizeCase [^String sql])
      (^String Widen         [^String sql])))

#?(:cljs
    (defn ^:export tokenizeAndStandardize
      [sql jsObj_options]
      (let [options (set/rename
                      (js->clj jsObj_options)
                      (zipmap (map name option-keys) option-keys))]
        (tokenize-and-standardize (str sql) options)))
   :default
    (defn tokenizeAndStandardize
      [sql options]
      (tokenize-and-standardize (str sql) options)))

#?(:cljs
    (defn ^:export tokenizeAndStandardizeCase
      [sql jsObj_options]
      (let [options (set/rename
                      (js->clj jsObj_options)
                      (zipmap (map name option-keys) option-keys))]
        (standardize-case (str sql) options)))
   :default
    (defn tokenizeAndStandardizeCase
      [sql options]
      (standardize-case (str sql) options)))

(defn ^:export standardizeWide
  [s]
  (string/trim (util/canonical-whitespace s)))

#?(:cljr
    (defrecord SqlNormalizer [options]; :load-ns true
      ISqlNormalizer
      (^String Normalize     [this ^String sql]
        (tokenizeAndStandardize     sql (.-options this)))
      (^String NormalizeCase [this ^String sql]
        (tokenizeAndStandardizeCase sql (.-options this)))
      (^String Widen [this ^String sql]
        (standardizeWide sql))))
