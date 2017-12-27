(ns cecil.cli
  (:require [clojure.string :as string]
            [cecil.ccl-to-sql :as cts]))

#?(:cljs (defn slurp [file-name] "cannot slurp in cljs"))
#?(:cljs (defn spit [file-name contents] "cannot spit in cljs"))

(defn convert-file!
  [ccl-file sql-file]
  (let [ccl (slurp ccl-file)
        [sql report] (cts/ccl->sql-and-report ccl)]
    (spit sql-file sql)
    (println report)
    sql-file))

(defn -main [& ccl-files]
  (doseq [ccl-file ccl-files
          :let [sql-file (string/replace ccl-file #"\.[^.]+$" ".sql")]]
    (println "==========================================================")
    (println (str "Converting " ccl-file " into " sql-file))
    (convert-file! ccl-file sql-file)))
