(ns cecil.ccl-to-sql
  (:require
   [clojure.string :as string]
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
