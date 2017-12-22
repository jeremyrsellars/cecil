(ns cecil.util-test
  (:require
   #_[om.core :as om :include-macros true]
   [clojure.string :as string]
   [clojure.test :refer [testing is]]
   [clojure.pprint :as pprint]
   [sablono.core :as sab :include-macros true]
   [cecil.cki :as cki]
   [cecil.util :as util])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]))

(deftest tokenized-correctly
  (letfn [(is-tokenized-correctly [s tokens]
            (testing s
              (is (= tokens (util/tokenize s)))))
          (is-tokenized-correctly2 [tokens]
            (is-tokenized-correctly
              (string/join "" tokens)
              tokens))]

    (is-tokenized-correctly
       "uar_get_code_by(\"MEANING\",4500,\"INPATIENT\")"
       ["uar_get_code_by" "(" "\"MEANING\"" "," "4500" "," "\"INPATIENT\"" ")"])

    (is-tokenized-correctly
       "1 --uar_get_code_by(\"MEANING\",4500,\"INPATIENT\")"
       ["1" " " "--uar_get_code_by(\"MEANING\",4500,\"INPATIENT\")"])

    (is-tokenized-correctly
       "1 /*uar_get_code_by\r\n(\"MEANING\",4500,\"INPATIENT\")  */"
       ["1" " " "/*uar_get_code_by\r\n(\"MEANING\",4500,\"INPATIENT\")  */"])

    (is-tokenized-correctly2
       ["select" " " "1" " " "from" " " "x" " " "group" "group by" " " "1" " " "order      by" " " "1"])))


(deftest whitespace-canonicalized-correctly
  (letfn [(is-canonicalized-correctly [s expected]
            (is (= expected (util/canonical-whitespace s))
                (str "'" s "'")))]

    (is-canonicalized-correctly
      ""
      "")

    (is-canonicalized-correctly
      " "
      " ")

    (is-canonicalized-correctly
      "          "
      " ")


    (is-canonicalized-correctly
      "  \n  1  \n    "
      " 1 ")

    (is-canonicalized-correctly
      "1 uar_get_code_by\r\n(  \"MEANING\"  ,  4500  ,  \"INPATIENT\"  )  /*  comments-unchanged */--"
      "1 uar_get_code_by ( \"MEANING\" , 4500 , \"INPATIENT\" ) /*  comments-unchanged */--")))
