(ns jdsl.basic-test
  (:require [clojure.test :refer [deftest testing is]]
            [jdsl.basic :as jb]
            [jdsl.char-stream :as cs]
            [jdsl.char-parser :as jp]
            [jdsl.combinator :as jc]
            [clojure.string :as str]))

(deftest basic-test
  (testing "error?"
    (is (= (jb/error? nil) true))
    (is (= (jb/error? "Expected: char") true))
    (is (= (jb/error? [(vec "input") -1]) false)))
  (testing "error"
    (is (= (jb/error nil) nil))
    (is (= (jb/error "Expected: char") "Expected: char"))
    (is (= (jb/error "Expected: " "char") "Expected: char")))
  (testing "ok"
    (is (= (jb/ok \a [(vec "input") 1]) [\a [(vec "input") 1]])))
  (testing "EOS?"
    (is (= (jb/EOS? nil) true))
    (is (= (jb/EOS? \a) false)))
  (testing "print-error and parse-error"
    (let [e (jb/parse-error "Expected: a" (cs/create "input"))
          x (str "ParseError \n "
                 "Expected: a \n "
                 "   Found: i\r\n")]
    (is (= (str/trim x) (str/trim (with-out-str (jb/print-error e)))))))
  (testing "run"
    (is (= (jb/run (jp/char \a) (cs/create "abc")) [\a [(vec "abc") 0 3]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/char \b) (cs/create "abc")))))
  (testing "attempt"
    (is (= (jb/attempt (jp/char \a) (cs/create "abc")) [\a [(vec "abc") 0 3]]))
    (is (= (jb/attempt (jp/char \b) (cs/create "abc")) [nil [(vec "abc") -1 3]])))
  (testing "do"
    (let [p (jb/do
              (a <- jp/any-char)
              (b <- (jp/char \b))
              jp/skip-any-char
              (jp/skip-any-char)
              (e <- (jc/attempt (jc/$> jp/eof :eof)))
              (if (= e :eof) 
                (jc/return [a b :eof])
              (jc/return [a b])))]
    (is (= (jb/run p (cs/create "abce")) [[\a \b :eof] [(vec "abce") 3 4]]))
    (is (= (jb/run p (cs/create "abcef")) [[\a \b] [(vec "abcef") 3 5]]))))
  (testing "do"
    (let [p (jb/do
              (_ <- jp/any-char)
              (jp/any-char))]
    (is (= (jb/run p (cs/create "abcef")) [\b [(vec "abcef") 1 5]]))))
  (testing "do"
    (let [p (jb/do
              (_ <- jp/any-char)
              jp/any-char)]
    (is (= (jb/run p (cs/create "abcef")) [\b [(vec "abcef") 1 5]]))))
  (testing "do"
    (let [p (jb/do
              (_ <- jp/any-char)
              (jp/char \b))]
    (is (= (jb/run p (cs/create "abcef")) [\b [(vec "abcef") 1 5]]))))
  (testing "do"
    (let [p (jb/do
              (jp/char \a))]
    (is (= (jb/run p (cs/create "abcef")) [\a [(vec "abcef") 0 5]])))))
