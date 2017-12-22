(ns cecil.util
  (:require [clojure.string :as string]))

(def tokens-regex
  #"(?i)\"(?:\"\"|[^\"]+)+\"|'(?:''|[^']+)+'|--[^\r\n]*|;[^\r\n]*|/\*(?:(?!\*/)[\s\S])*\*/|\s+|!=|order\s+by|group(?:\s+by)?|\w+|.")
  ; ccl also uses ;`` as a line comment

(defn tokenize
  [s]
  (re-seq tokens-regex s))

(defn canonical-whitespace
  [s]
  (string/replace s tokens-regex #(if (string/blank? %) " " %)))
