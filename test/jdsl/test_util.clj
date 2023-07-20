(ns jdsl.test-util
  (:require [clojure.test :refer [is]]))

(defmacro is-parse-error? [body]
  `(is (~(symbol "thrown-with-msg?") clojure.lang.ExceptionInfo #"ParseError" ~body)))
