(ns cecil.test-macros)

(defmacro insert-file-contents-string
  [file-name]
  (slurp file-name))
