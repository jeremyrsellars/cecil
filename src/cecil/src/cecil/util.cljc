(ns cecil.util
  (:require [clojure.string :as string]))

(def tokens-regex
  #"\"(?:\"\"|[^\"]+)+\"|'(?:''|[^']+)+'|--[^\r\n]*|/\*(?:(?!\*/)[\s\S])*\*/|\s+|\w+|.")

(defn tokenize
  [s]
  (re-seq tokens-regex s))

(defn canonical-whitespace
  [s]
  (string/replace s tokens-regex #(if (string/blank? %) " " %)))
