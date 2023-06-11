(ns jdsl.core
  (:require [jdsl.basic        :as jb]
            [jdsl.combinator   :as jc]
            [jdsl.flipped      :as jf]
            [jdsl.char-parser  :as jp]
            [jdsl.char-stream  :as cs]))

(set! *warn-on-reflection* true)

(def ba (jf/label "Expected: ba" (jf/<> (jp/char \a) (jp/char \b))))
(def ab
  (jf/label "Expected: ab"
  (jb/do
    (a <- (-> (jp/any-of "abc") (jc/label "Expected: any-of 'abc'")))
    (b <- (jp/char \b))
    (c <- (-> (jp/satisfy char?) (jc/label "Expected: int char")))
    (d <- jp/hex)
    (g <- (-> jp/eof (jc/$> :eof) (jc/label "Expected: eof")))
    (jc/return [a b c d g]))))

(defn -main
  [& args]
  (try
    (println (jb/run ab (cs/create "abcF")))
  (catch clojure.lang.ExceptionInfo e
    (jb/print-error e))))
