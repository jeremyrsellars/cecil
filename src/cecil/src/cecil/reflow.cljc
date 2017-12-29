(ns cecil.reflow
  (:require
   [clojure.set :as set]
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

(def ^:dynamic *break-parenthetical-length* 60)

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

(defn minimal-whitespace
  [s]
  (cond
    (nil? s)     nil
    (empty? s)   ""
    (string? s)  (string/replace s #"\s+" " ")))

(defn with-minimal-whitespace
  [{ws :leading-whitespace :as ast-node}]
  (if (empty? ws)
    (dissoc ast-node :leading-whitespace)
    (assoc ast-node :leading-whitespace (minimal-whitespace ws))))

(defn remove-leading-whitespace-in-first-node
  "Traverses the ast-node tree and removes all leading-whitespace on the path to the first leaf."
  [{:keys [nodes] :as ast-node}]
  (cond-> ast-node
    true
    (dissoc :leading-whitespace)

    (ast-node? (first nodes))
    (update-in [:nodes 0] remove-leading-whitespace-in-first-node)))


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
  (fn [ancestor-nodes {:keys [type] :as ast-node}]
    type))

(defmethod relative-indent :default
  [_ _]
  0)

(defmulti node-own-line?
  (fn [ancestor-nodes {:keys [type] :as ast-node}]
    type))

(defmethod node-own-line? :default
  [_ {:keys [type keyword] :as ast-node}]
  false)


(defmethod node-own-line? :expression
  [ancestor-nodes {:keys [type keyword sub-type] :as ast-node}]
  (case sub-type
    :parenthetical
    (if-let [{ancester-type :type :as ancestor} (first (filter ast-node? ancestor-nodes))]
      (not (= :function-invocation ancester-type))
      false)
    false))

(defmethod relative-indent :expression
  [ancestor-nodes ast-node]
  (or
    (when-let [{ancester-type :type :as ancestor} (first (filter ast-node? ancestor-nodes))]
      (cond
        (and (not= ancester-type :function-invocation)
             (is-parenthetical-expression? ast-node))
        2

        (node-own-line? ancestor-nodes ast-node)
        1))

    0))

(defmethod relative-indent :field-conjunction
  [_ _]
  1)

(defmethod relative-indent :field-definition
  [_ _]
  1)

(defmethod node-own-line? :field-definition
  [_ {:keys [type keyword] :as ast-node}]
  true)

(defmethod node-own-line? :group-by
  [ancestor-nodes {:keys [keyword] :as ast-node}]
  true)

(defmethod node-own-line? :order-by
  [ancestor-nodes {:keys [keyword] :as ast-node}]
  true)

(defmethod node-own-line? :keyword
  [ancestor-nodes {:keys [keyword] :as ast-node}]
  (or
    (contains? top-level-keywords keyword)
    (= keyword :and)
    (= keyword :or)))


(defmethod relative-indent :keyword
  [ancestor-nodes {:keys [keyword] :as ast-node}]
  (cond
    (contains? top-level-keywords keyword)
    0

    :default
    (if-let [{ancester-type :type :as ancestor} (first (filter ast-node? ancestor-nodes))]
      (if (and (= :select ancester-type) (= :distinct keyword))
        0
        0)
      nil)))

(defmethod node-own-line? :whitespace
  [_ {:keys [type keyword] :as ast-node}]
  false)


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

(defn insert-newline-after-commas-etc
  [{:keys [nodes] :as ast-node}]
  (if (> 2 (count nodes))
    ast-node
    (let [{:keys [type sub-type]} ast-node
          lazy-should-break-on-commas
          (delay (or (not= :expression type)
                     (not= :parenthetical sub-type)
                     (<= *break-parenthetical-length* (count (cts/emit-string ast-node)))))]
      (assoc ast-node
        :nodes
        (into []
          (cons (first nodes)
            (map
              (fn [{prev-indent :absolute-indent, prev-keyword :keyword :as prev-node} {:keys [absolute-indent] :as node}]
                (let [{:keys [own-line?] :as new-node}
                      (cond-> node
                        (and (token-of-type? prev-node :comma) @lazy-should-break-on-commas)
                        (assoc :own-line? true)

                        (and (some? prev-indent)
                             (not= absolute-indent prev-indent)
                             (not= prev-keyword :select))
                        (assoc :own-line? true))]
                  (cond-> new-node
                    own-line?
                    remove-leading-whitespace-in-first-node)))
              nodes
              (rest nodes))))))))

(defn parse-select
  [tokens]
  (let [[kw tokens] (next-token tokens)
        [distinct tokens] (let [[{:keys [type keyword] :as d} toks] (next-token tokens)]
                            (if (and (= :keyword type) (= keyword :distinct))
                              [(assoc d :leading-whitespace " ") toks]
                              [nil tokens]))
        [select-list-parsed tokens] (parse-select-list tokens)
        [next-expression remaining] (parse-expression tokens #(token-of-type? % :rparen))
        from-etc (update next-expression
                    :nodes (fn [nodes]
                              (map
                                (fn [{:keys [keyword type] :as node}]
                                  (cond-> node
                                    (not (contains? top-level-keywords (or keyword type)))
                                    (assoc :indent 1)))
                                nodes)))
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
                  (assert-ast-node from-etc)]))}
      remaining])))

(defn tokenize-and-parse
  [ccl]
  (let [tokens (util/tokenize ccl)]
   (assert-ast-node-and-tokens
    (parse-select tokens))))

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
          (+ absolute-indent
             current-indent
             (relative-indent ancestor-nodes ast-node)))
        (update :own-line? #(or % (node-own-line? ancestor-nodes ast-node)))
        insert-newline-after-commas-etc)))

(defn indention-walker-first-pass
  [ancestor-nodes x]
  (if (ast-node? x)
    (node-indent-first-pass ancestor-nodes x)
    x))

(defn node-indent-second-pass
  [ast-node]
  (insert-newline-after-commas-etc ast-node))

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
      (let [{:keys [absolute-indent indent own-line? type keyword]} x]
        ; (println :x (str \" (cts/emit-string (dissoc x :leading-whitespace)) \")
        ;   :kw  (some? keyword)
        ;   :ccl (contains? cts/ccl-keywords keyword)
        ;   :sl? (not own-line?))
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
          (assoc :leading-whitespace (string/join (cons nl-ws (repeat absolute-indent indent-ws)))))))))


(defn reflow
  [ast-node options]
  (binding [*break-parenthetical-length*
            (get options :break-parenthetical-length *break-parenthetical-length*)]
    (->> ast-node
         (prewalk-ancestry indention-walker-first-pass '())
         (walk/postwalk indention-walker-second-pass)
         (walk/prewalk
          (ws-walker
            (get options :indent "  ")
            (get options :new-line "\r\n"))))))

(defn tokenize-and-reflow
  [ccl options]
  (-> ccl
      string/trim
      tokenize-and-parse
      (reflow options)
      cts/emit-string))

(def option-keys
 [:indent
  :new-line
  :break-parenthetical-length])

#?(:cljs
    (defn ^:export tokenizeAndReflow
      [ccl jsObj_options]
      (let [options (set/rename
                      (js->clj jsObj_options)
                      (zipmap (map name option-keys) option-keys))]
        (tokenize-and-reflow (str ccl) options))))
