(ns cecil.util
  (:require [clojure.string :as string]))

(defn parse-int [^String s]
  #?(:cljr (Int64/TryParse s)
     :clj  (Long. s)
     :cljs (js/parseInt s)))

(defn parse-float [^String s]
  #?(:cljr (Double/TryParse s)
     :clj  (Double. s)
     :cljs (js/parseFloat s)))

(defn parse-number [^String s]
  (if (re-find #"[.eE]" s)
    (parse-float s)
    (parse-int s)))

(defn nil-safe-+
  [& addends]
  (reduce + 0 (keep identity addends)))

(def tokens-regex
  #"(?i)\"(?:\"\"|[^\"]+)*\"|'(?:''|[^']+)*'|--[^\r\n]*[\r\n]*|;[^\r\n]*[\r\n]*|/\*(?:(?!\*/)[\s\S])*\*/|\s+|\d+(?:\.\d+)?|!=|[<>]+=?|(?:inner|left|right|full)\s+(?:outer\s+)?join\b|order\s+by\b|group(?:\s+by)?\b|not\s+in|is\s+not|:?\w+|\|\||.")
  ; ccl also uses ;`` as a line comment

(defn tokenize
  [s]
  (re-seq tokens-regex s))

(defn canonical-whitespace
  [s]
  (string/replace s tokens-regex #(if (string/blank? %) " " %)))

(defn unwrap-string-double
  "Takes an SQL encoded-string token in the form of `\"value with \"\"quotes\"\".\"`
  and yields the decoded string: `value with \"quotes\".`."
  [encoded-string]
  (string/replace encoded-string #"^\"|\"$|\"\"" #(if (= "\"\"" %) "\"" "")))

(defn unwrap-string-single
  "Takes an SQL encoded-string token in the form of `'value with ''quotes''.'`
  and yields the decoded string: `value with 'quotes'.`."
  [encoded-string]
  (string/replace encoded-string #"^'|'$|''" #(if (= "''" %) "'" "")))

(defn truncate
  "Truncates s to maximum length n."
  [s n]
  (cond-> s
    (< n (count s))
    (subs 0 n)))
