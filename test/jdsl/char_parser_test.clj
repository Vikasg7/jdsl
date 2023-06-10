(ns jdsl.char-parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [jdsl.basic :as jb]
            [jdsl.char-stream :as cs]
            [jdsl.char-parser :as jp]))

(deftest char-parser-test
  (testing "char"
    (is (= (jb/run (jp/char \a) (cs/create "abc"))  [\a [(vec "abc") 0]]))
    (is (= (jb/run (jp/char \newline) (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/char \return) (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/char \return) (cs/create "\n")) [\newline [(vec "\n") 0]]))
    (is (= (jb/run (jp/char \newline) (cs/create "\r")) [\newline [(vec "\r") 0]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/char \a) (cs/create "\r\n")))))
  (testing "skip-char"
    (is (= (jb/run (jp/skip-char \a) (cs/create "abc"))  [nil [(vec "abc") 0]]))
    (is (= (jb/run (jp/skip-char \newline) (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/skip-char \return) (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/skip-char \return) (cs/create "\n")) [nil [(vec "\n") 0]]))
    (is (= (jb/run (jp/skip-char \newline) (cs/create "\r")) [nil [(vec "\r") 0]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/skip-char \a) (cs/create "\r\n")))))
  (testing "any-char"
    (is (= (jb/run jp/any-char (cs/create "abc"))  [\a [(vec "abc") 0]]))
    (is (= (jb/run jp/any-char (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run jp/any-char (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run jp/any-char (cs/create "\n")) [\newline [(vec "\n") 0]]))
    (is (= (jb/run jp/any-char (cs/create "\r")) [\newline [(vec "\r") 0]]))
    )
  )
