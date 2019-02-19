(ns cecil.cki-macros)

(defn parse-int [^String s]
  #?(:cljr (Int64/TryParse s)
     :clj  (Long. s)
     :cljs (parseInt s)))

(defn load-cki
  [file-name]
  (let [value-lines (rest (re-seq #"\"([^\"]+)\",\"([^\"]+)\",\"([^\"]+)\"" (slurp file-name)))]
    (reduce
      (fn [m [line code-set cdf-meaning cki]]
        (let [cs (str (parse-int code-set))
              m (if (contains? m cs) m (assoc m cs {}))]
          (update m cs assoc cdf-meaning cki)))
      (sorted-map)
      value-lines)))

#?(:cljs nil
   :default
    (defmacro cki-map
      [file-name]
      (load-cki file-name)))
