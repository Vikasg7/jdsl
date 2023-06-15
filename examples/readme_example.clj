(ns examples.readme-example
  (:require [jdsl.char-parser :as jp]
            [jdsl.char-stream :as cs]
            [jdsl.basic       :as jb]
            [jdsl.combinator :refer [between many*]]))

(def any-char-within-braces
  (-> (many* jp/alpha-num)
      (between (jp/char \[) (jp/char \]))))

(try
  (jb/run any-char-within-braces (cs/create "[abcd123]"))
(catch clojure.lang.ExceptionInfo e
  (jb/print-error e)))
