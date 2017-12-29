(ns cecil.reflow
  (:require
   [clojure.string :as string]
   [clojure.spec.alpha :as s]
   [clojure.pprint :as pprint]
   [clojure.walk :as walk]
   [cecil.cki :as cki]
   [cecil.ccl-to-sql :as cts :refer [assert-ast-node
                                     assert-ast-node-and-tokens
                                     assert-ast-nodes-and-tokens
                                     assert-ast-nodes
                                     canonical-keyword
                                     next-token
                                     token-of-type?]]
   [cecil.util :as util]))


(def top-level-keywords
 #{:select
   :from
   :where
   :plan
   :join
   :order
   :order-by
   :group-by
   :having})

(defmulti relative-indent
  (fn [ancestor-nodes {:keys [type] :as token}]
    type))

(defmethod relative-indent :default
  [_ {:keys [type keyword] :as token}]
  (if (and (= type :keyword) (contains? top-level-keywords keyword))
    0
    1))

(defmulti node-own-line?
  (fn [ancestor-nodes {:keys [type] :as token}]
    type))

(defmethod node-own-line? :field-definition
  [_ {:keys [type keyword] :as token}]
  true)

(defmethod node-own-line? :expression
  [_ {:keys [type keyword] :as token}]
  true)

(defmethod node-own-line? :whitespace
  [_ {:keys [type keyword] :as token}]
  false)

(defmethod node-own-line? :default
  [_ {:keys [type keyword] :as token}]
  (and (= type :keyword) (contains? top-level-keywords keyword)))


(declare parse-select)
(declare parse-expression)



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
        (when (token-of-type? nt :terminal)
          (pprint/pprint ["WARNING: unterminated parenthetical-expression:"
                          parts]))

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

(defn is-parenthetical-expression?
  [ast-node]
  (boolean
    (when (associative? ast-node)
      (let [{:keys [type sub-type nodes]} ast-node]
          (and (= sub-type :parenthetical)
               (= type :expression))))))

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
        :nodes [(assert-ast-node expression)]
        :expression expression}]
      rst))))

 ([field-definitions tokens]
  (let [[nt remaining] (next-token tokens)]
   (assert-ast-node-and-tokens
      (if (token-of-type? nt :equals)
        (let [[part2 remaining2] (parse-field-definition remaining)
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

(defn parse-select
  [tokens]
  (let [[kw tokens] (next-token tokens)
        [distinct tokens] (let [[{:keys [type keyword] :as d} toks] (next-token tokens)]
                            (if (and (= :keyword type) (= keyword :distinct))
                              [(assoc d :leading-whitespace " ") toks]
                              [nil tokens]))
        [select-list-parsed tokens] (parse-select-list tokens)
        [next-expression remaining] (parse-expression tokens #(token-of-type? % :rparen))
        from next-expression
        select-list
        {:type :select-list
         :nodes (assert-ast-nodes select-list-parsed)}]
   (assert-ast-node-and-tokens
     [{:type :select
       :nodes (into []
                (filter some?
                 [kw
                  distinct
                  select-list
                  (assert-ast-node from)]))}
      remaining])))

(defn tokenize-and-parse
  [ccl]
  (let [tokens (util/tokenize ccl)]
   (assert-ast-node-and-tokens
    (parse-select tokens))))

(defn prewalk-ancestry
  "Like clojure.walk/prewalk, but includes hiarchical ancestry."
  [f ancestry form]
  (walk/walk (partial prewalk-ancestry f (cons form ancestry)) identity (f ancestry form)))

(defn ast-node?
  [x]
  (and (map? x) (contains? x :type)))

(defn node-indent
  [ancestor-nodes ast-node]
  (let [{:keys [indent]} (some ast-node? ancestor-nodes)]
    (assoc ast-node
      :indent    (+ indent (relative-indent ancestor-nodes ast-node))
      :own-line? (node-own-line? ancestor-nodes ast-node))))



(defn indention-walker
  [ancestor-nodes x]
  (if (ast-node? x)
    (node-indent ancestor-nodes x)
    x))

(defn ws-walker
  [indent-ws nl-ws]
  (fn ws-walk
    [x]
    (if-not (ast-node? x)
      x
      (let [{:keys [indent own-line?]} x]
        ;(println :x \" (string/join (cons nl-ws (repeat indent indent-ws))) \")
        (cond-> x
          ; true
          ; (dissoc :indent :own-line?)

          (and own-line? (some? indent))
          (assoc :leading-whitespace (string/join (cons nl-ws (repeat indent indent-ws)))))))))


(defn reflow
  [ast-node]
  (->> ast-node
       (prewalk-ancestry indention-walker '())
       (walk/prewalk (ws-walker "  " "\r\n"))))

; (prewalk-ancestry #(do (println (:type (first %1))) %2) '() ex)

