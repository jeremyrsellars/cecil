(ns cecil.util
  (:require [clojure.string :as string]))

(def tokens-regex
  #"(?i)\"(?:\"\"|[^\"]+)*\"|'(?:''|[^']+)*'|--[^\r\n]*[\r\n]*|;[^\r\n]*[\r\n]*|/\*(?:(?!\*/)[\s\S])*\*/|\s+|\d+(?:\.\d+)?|!=|<>|(?:inner|left|right|full)\s+(?:outer\s+)?join\b|order\s+by\b|group(?:\s+by)?\b|:?\w+|.")
  ; ccl also uses ;`` as a line comment

(defn tokenize
  [s]
  (re-seq tokens-regex s))

(defn canonical-whitespace
  [s]
  (string/replace s tokens-regex #(if (string/blank? %) " " %)))
