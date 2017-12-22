(ns cecil.ccl-to-sql
  (:require
   [clojure.string :as string]
   [clojure.spec.alpha :as s]
   [cecil.cki :as cki]
   [cecil.util :as util]))

;; uar_get_code_by

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

(def translate-all
 (let [fns
         [translate-uar_get_code_by
          translate-uar_get_code_display
          translate-cnvtstring
          translate-cnvtreal
          translate-cnvtupper]]
  (apply comp fns)))


(defn ^:export translateAll
  [ccl]
  (translate-all ccl))

(defn  ^:export report
  [ccl]
  (let [matches (mapcat
                  #(let [ms (re-seq % ccl)]
                      (if (string? (first ms))
                       ms
                       (map first ms)))
                  regexes)
        substitutions (mapv
                       #(vector % (translate-all %))
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
(s/def ::nodes (s/coll-of ::ast-node-or-token))
(s/def ::token string?)
(s/def ::ast-node (s/keys :req-un [::type ::nodes]
                          :opt-un [::expression]))
(s/def ::ast-node-or-token (s/or :node ::ast-node :token ::token))
(s/def ::expression ::ast-node)
(s/def ::ast-nodes (s/coll-of ::ast-node))


(defn ast-node
  [x]
  (s/assert ::ast-node x))

(defn ast-nodes
  [x]
  (s/assert ::ast-nodes x))



(def ccl-keywords
 #{:select
   :from})

(def token->type
  {"." :dot
   "," :comma
   "=" :equals
   "(" :lparen
   ")" :rparen})


(defn string->token
  [s]
  (assoc
    (if-let [tt (get token->type s)]
      {:type tt}
      (if-let [canonical-keyword (ccl-keywords (keyword s))]
        {:type :keyword
         :keyword canonical-keyword}
        {:type (cond
                  (string/blank? s)         :whitespace
                  :else                     :identifier)}))
    :nodes [s]))    ; consider interning here


(defn parse-keyword
  [s]
  {:type :keyword
   :nodes [s]
   :canonical (keyword (string/lower-case s))})

(defn token-of-type?
 ([{:keys [type]} kw]
  (= kw type))
 ([{:keys [type]} kw & types]
  (or (= kw type)
      (some #(= type %) types)
      false)))

(defmulti valid-string?
  (fn assert-valid-string-dispatch
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

(defmethod valid-string? :whitespace
  [_ s]
  (and (string? s)
       (string/blank? s)))

(defmethod valid-string? :comma
  [_ s]
  (and (string? s)
       (string/blank? s)))

(declare parse-select)
(declare parse-expression)

(defn next-token
  [[t1 & rst :as tokens]]
  (if (empty? tokens)
    nil
    (let [ws? (valid-string? :whitespace t1)]
      (if ws?
        (assoc-in (next-token rst) [0 :leading-whitespace] t1)
        [(string->token t1)
         rst]))))

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
          (nil? nt)
          [{:type :expression :sub-type :parenthetical
            :nodes (ast-nodes parts)}
           remaining]

          (token-of-type? nt :rparen)
          [{:type :expression :sub-type :parenthetical
            :nodes (ast-nodes (conj parts nt))}
           remaining]

          (token-of-type? nt :comma)
          (recur
            (conj parts nt)
            remaining)

          (and (token-of-type? nt :keyword) (-> nt :keyword (= :select)))
          (let [[sel tokens] (parse-select tokens)]
            (recur
              (conj sel)
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
        (some #(% nt) terminal-fns)
        [{:type :expression
          :nodes (ast-nodes parts)}
         tokens] ; don't eat terminal

        ; non-terminal
        :else
        (let [[expr remaining]
              (if (token-of-type? nt :lparen)
                (parse-parenthetical tokens)
                (parse-identifier tokens))]
          (recur
            (conj parts (ast-node expr))
            remaining))))))

(defn parse-field-definition
 ([tokens]
  (let [[expression rst] (parse-expression tokens #(token-of-type? % :equals :comma)
                                                  #(-> % :keyword (= :from)))]

    (parse-field-definition
      [{:type :field-definition
        :nodes [(ast-node expression)]
        :expression expression}]
      rst)))

 ([field-definitions tokens]
  (let [[nt remaining] (next-token tokens)]
      (if (token-of-type? nt :equals)
        (let [[part2 remaining2] (parse-field-definition remaining)
              ;[part2 remaining3] (parse-field-definition remaining2)
              {:keys [nodes], alias :expression} (first field-definitions)]
          [{:type :field-definition
            :nodes (ast-nodes (into (conj nodes nt) (get part2 :nodes)))
            :alias alias
            :expression (get part2 :expression)}
           remaining2])
        [(first (ast-nodes field-definitions)) tokens]))))

(defn parse-select-list
 ([tokens] ; parse the first field definition, then recurse
  (let [[fd remaining] (parse-field-definition tokens)]
    (parse-select-list [fd] remaining)))
 ([field-definitions tokens]
  (let [[nt remaining] (next-token tokens)]
      (if (token-of-type? nt :comma)
        (let [[fd remaining2] (parse-field-definition remaining)]
          (parse-select-list
            (conj field-definitions
              (assoc nt :type :field-conjunction)
              fd)
            remaining2))
        [field-definitions tokens]))))



(defn parse-select
  [[t1 ws & rst :as tokens]]
  (let [kw (parse-keyword (assert-valid-string :select t1))
        [select-list-parsed remaining] (parse-select-list rst)
        select-list
        {:type :select-list
         :leading-whitespace (assert-valid-string :whitespace ws)
         :nodes select-list-parsed}]
     [{:type :select
       :nodes [kw
               select-list]}
      remaining]))

(defn tokenize-and-parse
  [ccl]
  (let [tokens (util/tokenize ccl)
        [select-ast remaining] (parse-select tokens)]
    [[select-ast]
     remaining]))
