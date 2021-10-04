(ns cecil.honey
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [cecil.standardize :as standardize]
            [cecil.util :as util]))

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

(defn parse-select
  [nodes]
  {:source 'parse-select
   :nodes nodes})

(defn- combine-keywords
  [kw-nodes]
  (->> kw-nodes
       :nodes
       (pr-str);(keep :keyword)
       ; (map name)
       ; (string/join "-")
       ; keyword
       str))

(defn flatten-tokens
  [node]
  (->> node
    (walk/postwalk
      (fn transcode-walk
        [x]
        (if-not (standardize/ast-node? x)
          x
          (:nodes x))))
    flatten))

(defn- parse-identifier-kw
  [{:keys [nodes sub-type] :as node}]
 ;{'parse-identifier-kw node :result
  (->> node flatten-tokens string/join keyword))

(defn- raw
  [{:keys [nodes] :as node}]
 ;{'parse-identifier-kw node :result
  (->> node flatten-tokens (string/join " ")
    (vector :raw)))

(defmulti parse-expression-node
  (fn parse-expression-node_dispatch [node]
    (select-keys node [:type :sub-type :keyword])))

(defmethod parse-expression-node :default
  [node]
  (raw {:type :expression
        :nodes [node "pen1"]}))

(defmethod parse-expression-node {:type :identifier, :sub-type :composite}
  [node]
  (parse-identifier-kw (:nodes node)))

(defn- parse-expression-nodes
 ([nodes]
  (map parse-expression-node nodes)))

(defn- parse-field-definition
  [{:keys [nodes] :as fd-node}]
; {'parse-field-definition
  (let [[expr as alias] (partition-by #(token-of-keyword? % :as) nodes)]
    [(apply parse-expression-nodes expr)
     (parse-identifier-kw
        (or (first alias) fd-node))]))
     ;:fd-node fd-node]))

(defn- parse-select-list
  [{:keys [nodes]}]
  (into []
    (keep #(cond (token-of-type? % :field-conjunction :comma)              nil
                 (token-of-type? % :field-definition)                      (apply parse-field-definition (:nodes %))
                 :default                                                  %))
    nodes))

(defn parse-join
  [nodes]
  (raw {:nodes nodes :type :raw}))
  ;(pr-str nodes))

(defmulti assoc-parsed-clause (fn assoc-parsed-clause_dispatch [q clause-kw nodes] clause-kw))

(defn assoc-parsed-join
  [q clause-kw nodes]
  (update q
    :join-by
    (fn [jb]
      (conj (or jb [])
        clause-kw
        (parse-join nodes)))))


(defmethod assoc-parsed-clause :default
  [q clause-kw nodes]
  (if (re-find #"join" (name clause-kw))
    (assoc-parsed-join q clause-kw nodes)
    (assoc q clause-kw
      (raw {:nodes nodes :type :raw}))))

(defn- parse-from-etc
  [nodes]
  (let [top-clauses
        (partition-all 2
          (partition-by
            #(or (token-of-type? %    :from :join :group-by :having :order-by)
                 (token-of-keyword? % :from :join :group-by :having :order-by))  ; cheating to save time and future-proof tokenization
            nodes))]
    (reduce
      (fn assoc-clause [q [key-nodes val-nodes]]
        (let [clause-kw
              (-> (string/join "-" (flatten-tokens key-nodes))
                  (string/replace #" " "-")
                  string/lower-case
                  keyword)]
          (prn 'assoc-clause clause-kw :key-nodes key-nodes)
          (prn 'assoc-clause :val-nodes val-nodes)
          ;q #_
          (assoc-parsed-clause q clause-kw val-nodes)))
      {}
       ;:nodes nodes
       ;:top-clauses top-clauses}
      top-clauses)))

(defn- parse-selects
  [{select-nodes :nodes :as select-node}]
  (let [[[select distinct] select-list-etc] (split-with #(token-of-keyword? % :select :distinct) (:nodes select-node))
        select-kw (keyword (string/join "-" (map name (keep :keyword [select distinct]))))
        [select-list expression] select-list-etc
        query-set-op-kw? (fn query-set-op-kw? [n] (not (token-of-keyword? n :all :intersect :union :except)))
        [q1-from set-op_rest] (split-with             query-set-op-kw?  (:nodes expression))
        [set-op  -rest]       (split-with (complement query-set-op-kw?) set-op_rest)

        query
        (merge {select-kw (parse-select-list select-list)}
               (parse-from-etc q1-from))]
    (if (empty? set-op)
      query
      (let [set-op-kw (->> set-op flatten-tokens (string/join "-") keyword)]
        {;:set-op_rest set-op_rest
         ;:-rest -rest
         set-op-kw [query (parse-selects {:type ::additional-select :nodes -rest})]}))))


; (defn flatten-selects-expr
;   [x]
;   (let [nodes-to-expand (-> x :nodes first :nodes)
;         expanded (mapcat identity
;                    (for [{:keys [nodes] :as n} nodes-to-expand]
;                       (if (not (token-of-type? n :expression));(or true (string? (first nodes)))
;                         [n]
;                         nodes)))
;         selects (parse-selects expanded)]
;     {'flatten-selects-expr expanded
;      :selects selects}))


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

(defn- parse-join-kw
  [s]
  (let [normalized (string/upper-case s)]
    (case normalized
      "JOIN"        :join
      "LEFT JOIN"   :left-join
      "RIGHT JOIN"  :right-join
      "INNER JOIN"  :join
      "OUTER JOIN"  :outer-join
      "FULL JOIN"   :full-join
                    s)))

(defn- parse-function-invocation
  [& nodes]
  (prn :parse-function-invocation nodes)
 (let [[fn-ident [lparen args rparen]] nodes]
  (prn :parse-function-invocation fn-ident :args args)
  (into [fn-ident]
    (keep #(cond (= ::comma %)              nil
                 :default %))
    args)));(-> args rest butlast))))

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
        ;(parse-selects (:nodes n2)))
        (first (:nodes n2)))
    (into []
      (keep #(cond (= ::as %)              nil
                   :default %))
      ast-nodes)))

(defn- unwrap-string
  [wrapped-string]
  (string/replace wrapped-string #"[\"']" ""))

(defn- summary-string
  [dom]
  (with-out-str
    (prn dom)
    (println)
    (println "; -------------------------------------------------")
    (println)
    (pprint/pprint dom)))

(defn- transcode-honey
  [{:keys [type sub-type] :as ast-node}]
 (let [clean-node
       (walk/postwalk
          (fn remove-whitespace-meta [x]
            (cond-> x
              (map? x)
              (dissoc x :indent :absolute-indent :own-line? #_:leading-whitespace)))
          ast-node)]
  (->> clean-node
     ; (prewalk-ancestry indention-walker-first-pass '())
     ; (walk/postwalk indention-walker-second-pass)

    (walk/prewalk
      (fn transcode-walk
        [x]
        (if-not (standardize/ast-node? x)
          x
          (let [{:keys [;absolute-indent indent own-line?
                        type sub-type

                        ;leading-whitespace
                        nodes]} x
                replacement
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

                      (and (= type :select))
                      (parse-selects x)

                      ; (and (= type :select-list))
                      ; (apply parse-select-list nodes)

                      ; (and (= type :function-invocation))
                      ; (apply parse-function-invocation nodes)

                      ; (and (= type :join))
                      ; (apply parse-join-kw nodes)

                      ; (and (= type :expression) (= sub-type :parenthetical))
                      ; (do (prn :parenthetical-expression x)(apply parse-parenthetical-expression nodes))

                      ; (= type :expression)
                      ; (do (prn :expression x)(apply parse-expression-nodes nodes))

                      ; (and (= type :field-definition))
                      ; (apply parse-field-definition nodes)


                      :default
                      (do (prn x)
                        (dissoc x :indent :absolute-indent :own-line? :leading-whitespace)))]
            (prn ::x x '=> replacement)
            replacement))))
    ((cond (and (= type :expression) (= sub-type :with)) parse-with
           ;(= type :select)                              parse-selects
           :default                                      (do (prn 'transcode-honey type sub-type) identity)))
    summary-string
    (str "\r\n\r\n---------------- Before -----------------\r\n\r\n" (if false (with-out-str (pprint/pprint clean-node)) (pr-str clean-node)) "\r\n\r\n----------------AFTER -----------------\r\n\r\n"))))

(defn tokenize-and-honeyize
  [sql options]
  (let [[ast remaining]
        (-> sql
            string/trim
            standardize/tokenize-and-parse
            (standardize/standardize options))]
    ;(prn 'tokenize-and-honeyize :ast ast)

    (cond-> (transcode-honey ast)
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
