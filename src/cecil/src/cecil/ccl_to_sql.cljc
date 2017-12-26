(ns cecil.ccl-to-sql
  (:require
   [clojure.string :as string]
   [clojure.spec.alpha :as s]
   [clojure.pprint :as pprint]
   [clojure.walk :as walk]
   [cecil.cki :as cki]
   [cecil.util :as util]))

;; uar_get_code_by

(declare emit-string)

(def uar_get_code_by-regex
  #"(?i)\buar_get_code_by\s*\(\s*\"([^\"]+)\"\s*,\s*(\d+)\s*,\s*\"([^\"]+)\"\s*\)")

(defn translate-1-uar_get_code_by
  [ccl]
  (if-let [[_ type code-set k] (re-find uar_get_code_by-regex ccl)]
    (if-let [cki-str (and (= "MEANING" type) (get-in cki/cki [code-set k]))]
      (str "(select CODE_VALUE from CODE_VALUE cv where cv.cki = '" cki-str "' and ACTIVE_IND = 1 /*" ccl "*/)")
      (str "(select CODE_VALUE from CODE_VALUE where CDF_MEANING = '" k "' and CODE_SET = " code-set " and ACTIVE_IND = 1 /*" ccl "*/)"))
    ccl))

(defn translate-uar_get_code_by
  [ccl]
  (string/replace ccl uar_get_code_by-regex
    (fn [[s]]
      (translate-1-uar_get_code_by s))))

;; uar_get_code_display
(def uar_get_code_display-regex
  #"(?i)\buar_get_code_(description|display)\s*\(([^()]+)\)")

(defn translate-1-uar_get_code_display
  [ccl]
  (if-let [[_ field code-value] (re-find uar_get_code_display-regex ccl)]
    (str "(SELECT " (string/upper-case field) " FROM CODE_VALUE WHERE CODE_VALUE = " (string/replace code-value "\"" "'") " AND ACTIVE_IND = 1 /*" ccl "*/)")
    ccl))

(defn translate-uar_get_code_display
  [ccl]
  (string/replace ccl uar_get_code_display-regex
    (fn [[s]]
      (translate-1-uar_get_code_display s))))

;; cnvtstring
(def cnvtstring-regex
  #"(?i)\bcnvtstring\s*\(([^(),]+)(?:\s*,\s*\d+\s*,\s*\d+\s*)?\)")

(defn translate-1-cnvtstring
  [ccl]
  (if-let [[_ field] (re-find cnvtstring-regex ccl)]
    (str "TO_CHAR(" field ") /*" ccl "*/")
    ccl))

(defn translate-cnvtstring
  [ccl]
  (string/replace ccl cnvtstring-regex
    (fn [[s]]
      (translate-1-cnvtstring s))))

;; cnvtreal
(def cnvtreal-regex
  #"(?i)\bcnvtreal\s*\(([^()]+)\)")

(defn translate-1-cnvtreal
  [ccl]
  (if-let [[_ value] (re-find cnvtreal-regex ccl)]
    (str "(CASE WHEN " value " NOT LIKE '%[^0-9]%' THEN CAST(" value " as bigint) ELSE NULL END /*" ccl "*/)")
    ccl))

(defn translate-cnvtreal
  [ccl]
  (string/replace ccl cnvtreal-regex
    (fn [[s]]
      (translate-1-cnvtreal s))))

;; cnvtupper
(def cnvtupper-regex
  #"(?i)\bcnvtupper\s*\(([^()]+)\)")

(defn translate-1-cnvtupper
  [ccl]
  (if-let [[_ value] (re-find cnvtupper-regex ccl)]
    (str "UPPER(" value ") /*" ccl "*/)")
    ccl))

(defn translate-cnvtupper
  [ccl]
  (string/replace ccl cnvtupper-regex
    (fn [[s]]
      (translate-1-cnvtupper s))))



(def regexes
 [uar_get_code_by-regex
  uar_get_code_display-regex
  cnvtstring-regex
  cnvtreal-regex
  cnvtupper-regex])

(def replace-all
 (let [fns
         [translate-uar_get_code_by
          translate-uar_get_code_display
          translate-cnvtstring
          translate-cnvtreal
          translate-cnvtupper]]
  (apply comp fns)))


(defn  ^:export report
  [ccl]
  (let [matches (mapcat
                  #(let [ms (re-seq % ccl)]
                      (if (string? (first ms))
                       ms
                       (map first ms)))
                  regexes)
        substitutions (mapv
                       #(vector % (replace-all %))
                        matches)]
    (string/join "\r\n"
     (flatten
      ["/**************************************"
       " * Conversion Report"
       (str " * " (count substitutions) " substitutions")
       " * In no particular order:"
       " */"
       ""
       (map
          (fn [[before after]]
            ["Before: " before ""
             "After: " after "" "-------------------------------------------------------------" ""])
          substitutions)]))))

(defn about-regexes
  []
  (map
    #(-> % str
        (string/replace #"/\\b|\\s.*" ""))
    regexes))


;; CCL Parsing

(s/check-asserts true)

(s/def ::type keyword?)
(s/def ::nodes (s/coll-of ::ast-node-or-token
                      :kind #(and (sequential? %) (not (map? %)))
                      :into []))
(s/def ::token string?)
(s/def ::tokens (s/coll-of ::token :into []))
(s/def ::leading-whitespace (s/nilable string?))
(s/def ::ast-node (s/keys :req-un [::type ::nodes]
                          :opt-un [::expression ::leading-whitespace]))
(s/def ::ast-node-or-token (s/or :node ::ast-node :token ::token))
(s/def ::expression ::ast-node)
(s/def ::ast-nodes (s/coll-of ::ast-node
                      :kind #(and (sequential? %) (not (map? %)))
                      :into []))
;; parsing return types
(s/def ::ast-node-and-tokens (s/cat :return ::ast-node :tokens ::tokens))
(s/def ::ast-nodes-and-tokens (s/cat :return ::ast-nodes :tokens ::tokens))

(defn assert-ast-node
  [x]
  ; (pprint/pprint (s/explain-data ::ast-node x))
  (s/assert ::ast-node x))

(defn assert-ast-nodes
  [x]
  ; (pprint/pprint (s/explain-data ::ast-nodes x))
  (s/assert ::ast-nodes x))

(defn assert-ast-node-and-tokens
  [x]
  (s/assert ::ast-node-and-tokens x))

(defn assert-ast-nodes-and-tokens
  [x]
  (s/assert ::ast-nodes-and-tokens x))



(def ccl-keywords
 #{:select
   :from
   :and
   :or
   :where
   :plan
   :join
   ;:order :group :by  ; somehwere else because of whitespace
   :having})


(def unomynous-token->type
  {"." :dot
   "," :comma
   "=" :equals
   "!=" :not-equals
   "(" :lparen
   ")" :rparen})

(defn token->type
  [s]
  (or
    (get unomynous-token->type s)
    (cond
      (re-find #"(?i)group(?:\s+by)?" s) :group-by
      (re-find #"(?i)order\s+by" s)      :order-by
      (re-find #"^\d" s)                 :number)))


(defn string->token
  [s]
  (if (nil? s)
    {:type :terminal
     :nodes []}
    (assoc
      (if-let [tt (token->type s)]
        {:type tt}
        (if-let [canonical-keyword (ccl-keywords (keyword s))]
          {:type :keyword
           :keyword canonical-keyword}
          {:type (cond
                    (string/blank? s)         :whitespace
                    :else                     :identifier)}))
      :nodes [s])))    ; consider interning here


(defn parse-keyword
  [s]
  {:type :keyword
   :nodes [s]
   :keyword (keyword (string/lower-case s))})

(defn token-of-type?
 ([{:keys [type]} kw]
  (= kw type))
 ([{:keys [type]} kw & types]
  (or (= kw type)
      (some #(= type %) types)
      false)))

(defmulti valid-string?
  (fn valid-string-dispatch
    [kw value]
    kw))

(defmethod valid-string? :default
  [kw s]
  (= (name kw)
     (string/lower-case s)))


(defmulti valid-token?
  :type)

(defmethod valid-token? :default
  [{:keys [values type]}]
  (valid-string? type (first values)))


(defn assert-valid-string
  [kw s]
  (assert (valid-string? kw s))
  s)

(defn assert-valid-token
  [kw token]
  (assert (valid-token? kw token))
  token)

(defmethod valid-string? :whitespace
  [_ s]
  (and (string? s)
       (string/blank? s)))

(defmethod valid-string? :comma
  [_ s]
  (and (string? s)
       (string/blank? s)))

(defmethod valid-string? :comment
  [_ s]
  (and (string? s)
       (or (string/starts-with? s "--")
           (string/starts-with? s "/*")
           (string/starts-with? s ";"))))



(defn is-whitespace-or-comment
  [s]
  (or (valid-string? :whitespace s)
      (valid-string? :comment s)))

(defn canonical-whitespace-and-comments
  [s]
  (util/canonical-whitespace ; a second-pass for consecutive tokens
    (string/replace s util/tokens-regex
     #(if (is-whitespace-or-comment %)
        " "
        %))))


(declare parse-select)
(declare parse-expression)

(defn next-token
  [[t1 & rst :as tokens]]
  (let [ws? (or (valid-string? :whitespace t1)
                (valid-string? :comment t1))]
    (if ws?
      (assoc-in (next-token rst) [0 :leading-whitespace] t1) ; remember, next-token is [t rest]
      [(string->token t1)
       (vec rst)])))

(defn next-whitespaces-and-comments-as-string
  [tokens]
  (let [[ws-or-comments remaining] (split-with is-whitespace-or-comment tokens)]
    nil
    [(string/join "" ws-or-comments)
     (vec remaining)]))

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
            {:type :identifier :sub-type :composite
             :nodes (vec expression-definitions)})
         tokens]))))

(defn parse-parenthetical
  "Returns [expression remaining-tokens]"
  [tokens]
  (let [[nt tokens] (next-token tokens)]
    (loop [parts [nt]
           tokens tokens]
      (let [[nt remaining] (next-token tokens)]
        (cond
          (token-of-type? nt :rparen :terminal)
          [{:type :expression :sub-type :parenthetical
            :nodes (conj parts nt)}
           remaining]

          (token-of-type? nt :comma)
          (recur
            (conj parts nt)
            remaining)

          (and (token-of-type? nt :keyword)
               (-> nt :keyword (= :select)))
          (let [[sel tokens] (parse-select tokens)]
            (recur
              (conj parts sel)
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
        [{:type :expression
          :nodes (assert-ast-nodes parts)}
         tokens] ; don't eat terminal

        ; non-terminal
        :else
        (let [[expr remaining]
              (if (token-of-type? nt :lparen)
                (parse-parenthetical tokens)
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
        :nodes [(assert-ast-node expression)]
        :expression expression}]
      rst))))

 ([field-definitions tokens]
  (let [[nt remaining] (next-token tokens)]
   (assert-ast-node-and-tokens
      (if (token-of-type? nt :equals)
        (let [[part2 remaining2] (parse-field-definition remaining)
              ;[part2 remaining3] (parse-field-definition remaining2)
              {:keys [nodes], alias :expression} (first field-definitions)]
          [{:type :field-definition
            :nodes (assert-ast-nodes (into (conj nodes nt) (get part2 :nodes)))
            :alias alias
            :expression (get part2 :expression)}
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

(defn table-aliases-map
  "Yields a map of alias to table {alias table} from the CCL FROM clause"
  [ast-nodes]
  (let [is-separator? (fn is-separator? [{:keys [type]}] (= type :comma))
        pairs (remove
                #(every? is-separator? %)
                (partition-by is-separator? ast-nodes))]
    (reduce
      (fn [m [table alias]]
        (assoc m (-> alias :nodes first) (assoc table :leading-whitespace " ")))
      {}
      pairs)))

(defn plan-sections
  [nodes]
  (loop [sections []
         nodes nodes]
    (if (empty? nodes)
      sections
      (let [[{:keys [type keyword] :as n} & rst]  nodes]
        (if
          (and (= type :keyword)
               (or (= keyword :plan)
                   (= keyword :join)))
          (recur (conj sections [n])
                 rst)
          (recur (update sections (dec (count sections)) conj n)
                 rst))))))

(defn join-type
  [filter-expressions]
  (println ::to-do 'join-type "constantly interprets expressions as inner join:" #_filter-expressions)
  "inner join")

(defn interpret-plan
  [section-nodes table-aliases]
  (let [[{:keys [keyword] :as kw} alias-kw & raw-filter-expressions] section-nodes

        alias (-> alias-kw :nodes first)

        [expr-type filter-kw]
        (case keyword
          :plan [:from :where]
          :join [:join :on]
                [:unknown :unknown])

        filter-expressions
        (map
          (fn [{:keys [type keyword] :as ast-node}]
            (cond-> ast-node
              (and (= type :keyword) (= :where keyword))
              (assoc :nodes [(name filter-kw)])))
          raw-filter-expressions)

        table-def (get table-aliases alias alias-kw)
        ; _ (println :table-aliases table-aliases)
        ; _ (println :td alias '-> table-def)
        replacement-keyword
          (case keyword
            :plan "from"
            :join (join-type filter-expressions)
                  [:unknown :where])
        clause-start-nodes
          [(assoc kw :nodes [replacement-keyword])
           (assoc table-def :leading-whitespace " ")
           alias-kw]]
   (assert-ast-node
    {:type expr-type
     :nodes (case expr-type
              :from clause-start-nodes
              :join (into clause-start-nodes filter-expressions))
     :filter-expressions filter-expressions})))

(defn reinterpret-plans
  [nodes table-aliases]
  ; (println :nodes nodes)
  ; (println :plan-sections)
  ; (pprint/pprint (plan-sections nodes))
  (if-not (seq nodes)
    []
    (let [[from & joins] (map interpret-plan (plan-sections nodes) (repeat table-aliases))
          where (assoc from
                  :type :where
                  :nodes (get from :filter-expressions))
           ; _ (println :where where)
           ; _ (pprint/pprint from)
           ; _ (println :joins)
           ; _ (pprint/pprint joins)
           ;_ (println :fe (get from :filter-expressions))
           join-expressions
             (-> []
                 (conj (assert-ast-node (dissoc from :filter-expressions)))
                 (into (assert-ast-nodes (map #(dissoc % :filter-expressions) joins)))
                 (conj (assert-ast-node (dissoc where :filter-expressions))))]


      ; (pprint/pprint (->> ;(map update join-expressions (repeat :nodes) (repeat count))
      ;                     join-expressions
      ;                     (map #(dissoc % :filter-expressions))
      ;                     (vector :join-expressions)))
      ; (pprint/pprint (last join-expressions))
      (assert-ast-nodes join-expressions))))
      ;join-expressions)))

(defn reinterpret-from
  [{:keys [nodes] :as expression}]
  (let [is-from? (fn is-from? [{:keys [type keyword]}] (and (= type :keyword) (= keyword :from)))
        terminates-joins? (fn terminates-joins?
                            [{:keys [type keyword]}]
                            ;(println ::tj? type keyword)
                            (contains? #{:group-by :order-by :terminal} type))
        terminates-from? (fn terminates-from?
                            [{:keys [type keyword] :as ast-node}]
                            (or (and (= type :keyword)
                                     (= :plan keyword))
                                (terminates-joins? ast-node)))
        ast-nodes nodes
        [before-from from-etc] (split-with (complement is-from?) ast-nodes)
        ;_ (println :before-from before-from)
        [from aliases-etc] (split-with is-from? from-etc)
        ;_ (println :from from)
        [aliases after-from] (split-with (complement terminates-from?) aliases-etc)
        table-aliases (table-aliases-map aliases-etc)
        ; _ (println :table-aliases table-aliases)
        [joins after-joins] (split-with (complement terminates-joins?) after-from)
        ; _ (println :after-from after-from)
        ; _ (pprint/pprint [:after-joins after-joins])
        ; _ (pprint/pprint [:joins joins])
        join-expressions (reinterpret-plans joins table-aliases)
        ;_ (println :join-expressions join-expressions)

        ;_ (println :join-expressions join-expressions)
        nodes
        (-> []
            (into before-from)
            ;(into from)
            (into join-expressions)
            (into after-joins))]
   (assert-ast-node
    (assoc expression :nodes nodes))))

(defn maybe-reinterpret-from
  [{:keys [nodes] :as expression}]
  (if (some
       (fn [{:keys [type keyword]}]
          (and (= type :keyword)
               (= keyword :plan)))
       nodes)
    (reinterpret-from expression)
    expression))

(defn parse-select
  [tokens]
  (let [[kw tokens] (next-token tokens)
        [ws tokens] (next-whitespaces-and-comments-as-string tokens)
        [select-list-parsed tokens] (parse-select-list tokens)
        [next-expression remaining] (parse-expression tokens #(token-of-type? % :rparen))
        ; _ (println :select-list-parsed select-list-parsed)
        ;_ (println :ne (map :type (:nodes next-expression)))
        from (maybe-reinterpret-from next-expression)
        ;_ (println :from from)
        select-list
        {:type :select-list
         :leading-whitespace ws
         :nodes (assert-ast-nodes select-list-parsed)}]
     ; (pprint/pprint :from)
     ; (pprint/pprint from)
     ; (println ::parse-select (emit-string from))
   (assert-ast-node-and-tokens
     [{:type :select
       :nodes [kw
               select-list
               (assert-ast-node from)]}
      remaining])))

(defn tokenize-and-parse
  [ccl]
  (let [tokens (util/tokenize ccl)]
   (assert-ast-node-and-tokens
    (parse-select tokens))))

(letfn [(emit-leading-whitespace-and-tokens [x]
          (cond (map? x)          [(:leading-whitespace x) (:nodes x)]
                (string? x)       x
                (sequential? x)   x
                (nil? x)          nil
                :default          (println :elwat x)))]
  (defn emit-tokens
    [ast-nodes]
    (->> ast-nodes
         (walk/prewalk emit-leading-whitespace-and-tokens)
         flatten
         (filter some?)))

  (defn emit-string
    [ast-nodes]
    ;(println ::emit-string ast-nodes)
    (->> ast-nodes
         ; assert-ast-nodes
         emit-tokens
         (string/join ""))))

(let [as {:type :keyword
          :leading-whitespace " "
          :nodes ["AS"]}]
 (letfn [(alias-rearranger
           [{:keys [expression alias] :as n}]
           ;(println :alias-rearranger n)
           (let [pre-ws1 (get-in alias      [:nodes 0 :leading-whitespace])
                 pre-ws2 (get-in expression [:nodes 0 :leading-whitespace])
                 pre-ws (or pre-ws1 pre-ws2)
                 rearranged
                  (cond-> [(assoc-in expression [:nodes 0 :leading-whitespace] pre-ws)]
                    alias (conj as (assoc-in alias [:nodes 0 :leading-whitespace] " ")))]
            ;(println :alias-rearranged rearranged)
            (assoc n
              :nodes (assert-ast-nodes rearranged))))

         (change-alias
          [x]
          (if (and (map? x)
                   (= :field-definition (get x :type)))
            (assert-ast-node
              (alias-rearranger x))
            x))]


  (defn translate-field-aliases
    [ast-node-or-nodes]
    (->> ast-node-or-nodes
         (walk/prewalk change-alias)))))
         ; (walk/prewalk identity)))))
         ;(walk/prewalk identity))))

 ; (letfn [(is-parenthetical-expression?
 ;            [ast-node]
 ;            (if (associative? ast-node)
 ;              (let [{:keys [type sub-type nodes]} ast-node]
 ;                (boolean
 ;                  (and (= sub-type :parenthetical)
 ;                       (= type :expression))))
 ;              false))

 ;         (simplify-parenthetical
 ;           [{:keys [type sub-type nodes] :as outer}]
 ;           (let [inner (first nodes)
 ;                 pre-ws-outer (get outer :leading-whitespace)
 ;                 pre-ws-inner (get inner :leading-whitespace)
 ;                 pre-ws (or pre-ws-outer pre-ws-inner)]
 ;              (assoc inner :leading-whitespace pre-ws)))

 ;         (simplify-parenthetical-expression
 ;          [{:keys [type sub-type nodes] :as ast-node}]
 ;          (let [non-paren-nodes (remove #(token-of-type? % :lparen :rparen) nodes)]
 ;            (cond
 ;              (or (not (is-parenthetical-expression? ast-node))
 ;                  (not= 1 (count non-paren-nodes))
 ;                  (not (is-parenthetical-expression? (first non-paren-nodes))))
 ;              ast-node

 ;              :else
 ;              (simplify-parenthetical ast-node))))]

 ;  (defn simplify-parenthetical-expressions
 ;    [ast-nodes]
 ;    (->> ast-nodes
 ;         (walk/prewalk simplify-parenthetical-expression)
 ;         flatten
 ;         (filter some?)))))


; (defn simplify-sql
;   [sql]
;   (let [[ast remaining] (tokenize-and-parse sql)
;         simplified         (->> ast
;                                 simplify-parenthetical-expressions)
;         simplified-sql     (emit-string [simplified remaining])]
;     (replace-all simplified-sql)))


(defn ccl->sql-and-report
  [ccl]
  (let [[ast remaining] (assert-ast-node-and-tokens (tokenize-and-parse (replace-all ccl)))
        translated-ast  (-> ast
                            assert-ast-node
                            translate-field-aliases
                            assert-ast-node
                            identity)
        ; _ (pprint/pprint [:translated-ast translated-ast])
        ;translated-sql  (emit-string [ast remaining])
        translated-sql  (emit-string [translated-ast remaining])
        ; simplified-sql  (simplify-sql translated-sql)
        sql translated-sql]
    [sql
     (with-out-str
        (pprint/pprint
          (tokenize-and-parse sql)))]))

(defn ^:export translateAll
  [ccl]
  (let [[sql report] (ccl->sql-and-report ccl)]
    sql))
