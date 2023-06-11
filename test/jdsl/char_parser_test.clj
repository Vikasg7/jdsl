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
    (is (= (jb/run jp/any-char (cs/create "\r")) [\newline [(vec "\r") 0]])))
  (testing "skip-any-char"
    (is (= (jb/run jp/skip-any-char (cs/create "abc"))  [nil [(vec "abc") 0]]))
    (is (= (jb/run jp/skip-any-char (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run jp/skip-any-char (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run jp/skip-any-char (cs/create "\n")) [nil [(vec "\n") 0]]))
    (is (= (jb/run jp/skip-any-char (cs/create "\r")) [nil [(vec "\r") 0]])))
  (testing "satisfy"
    (is (= (jb/run (jp/satisfy (partial = \a)) (cs/create "abc"))  [\a [(vec "abc") 0]]))
    (is (= (jb/run (jp/satisfy (partial = \newline)) (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/satisfy (partial = \newline)) (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/satisfy (partial = \newline)) (cs/create "\n")) [\newline [(vec "\n") 0]]))
    (is (= (jb/run (jp/satisfy (partial = \newline)) (cs/create "\r")) [\newline [(vec "\r") 0]])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/satisfy (partial = \a)) (cs/create "\r\n"))))
  (testing "skip-satisfy"
    (is (= (jb/run (jp/skip-satisfy (partial = \a)) (cs/create "abc"))  [nil [(vec "abc") 0]]))
    (is (= (jb/run (jp/skip-satisfy (partial = \newline)) (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/skip-satisfy (partial = \newline)) (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/skip-satisfy (partial = \newline)) (cs/create "\n")) [nil [(vec "\n") 0]]))
    (is (= (jb/run (jp/skip-satisfy (partial = \newline)) (cs/create "\r")) [nil [(vec "\r") 0]])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/skip-satisfy (partial = \a)) (cs/create "\r\n"))))
  (testing "any-of"
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "abc"))  [\a [(vec "abc") 0]]))
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "\n")) [\newline [(vec "\n") 0]]))
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "\r")) [\newline [(vec "\r") 0]])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/any-of "a\r\n") (cs/create "bcd"))))
  (testing "skip-any-of"
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "abc"))  [nil [(vec "abc") 0]]))
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "\n")) [nil [(vec "\n") 0]]))
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "\r")) [nil [(vec "\r") 0]])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/skip-any-of "a\r\n") (cs/create "bcd"))))
  (testing "none-of"
    (is (= (jb/run (jp/none-of "bcd") (cs/create "abc"))  [\a [(vec "abc") 0]]))
    (is (= (jb/run (jp/none-of "bcd") (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/none-of "bcd") (cs/create "\r\n")) [\newline [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/none-of "bcd") (cs/create "\n")) [\newline [(vec "\n") 0]]))
    (is (= (jb/run (jp/none-of "bcd") (cs/create "\r")) [\newline [(vec "\r") 0]])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/none-of "a\r\n") (cs/create "abc"))))
  (testing "skip-none-of"
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "abc"))  [nil [(vec "abc") 0]]))
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "\r\n")) [nil [(vec "\r\n") 1]]))
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "\n")) [nil [(vec "\n") 0]]))
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "\r")) [nil [(vec "\r") 0]])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/skip-none-of "a\r\n") (cs/create "abc"))))
  (testing "lower"
    (is (= (jb/run jp/lower (cs/create "a"))  [\a [(vec "a") 0]]))
    (is (= (jb/run jp/lower (cs/create "z")) [\z [(vec "z") 0]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/lower (cs/create "A")))))
  (testing "upper"
    (is (= (jb/run jp/upper (cs/create "A"))  [\A [(vec "A") 0]]))
    (is (= (jb/run jp/upper (cs/create "Z")) [\Z [(vec "Z") 0]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/upper (cs/create "a")))))
  (testing "digit"
    (is (= (jb/run jp/digit (cs/create "0"))  [\0 [(vec "0") 0]]))
    (is (= (jb/run jp/digit (cs/create "9")) [\9 [(vec "9") 0]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/digit (cs/create "A")))))
  (testing "hex"
    (is (= (jb/run jp/hex (cs/create "0"))  [\0 [(vec "0") 0]]))
    (is (= (jb/run jp/hex (cs/create "9")) [\9 [(vec "9") 0]]))
    (is (= (jb/run jp/hex (cs/create "a")) [\a [(vec "a") 0]]))
    (is (= (jb/run jp/hex (cs/create "f")) [\f [(vec "f") 0]]))
    (is (= (jb/run jp/hex (cs/create "A")) [\A [(vec "A") 0]]))
    (is (= (jb/run jp/hex (cs/create "F")) [\F [(vec "F") 0]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/hex (cs/create "G"))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/hex (cs/create "g")))))
  (testing "octal"
    (is (= (jb/run jp/octal (cs/create "0"))  [\0 [(vec "0") 0]]))
    (is (= (jb/run jp/octal (cs/create "7")) [\7 [(vec "7") 0]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/octal (cs/create "8")))))
  (testing "tab"
    (is (= (jb/run jp/tab (cs/create "\t"))  [\tab [(vec "\t") 0]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/tab (cs/create " ")))))
  (testing "newline"
    (is (= (jb/run jp/newline (cs/create "\n"))  [\newline [(vec "\n") 0]]))
    (is (= (jb/run jp/newline (cs/create "\r\n"))  [\newline [(vec "\r\n") 1]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/newline (cs/create " ")))))
  (testing "skip-newline"
    (is (= (jb/run jp/skip-newline (cs/create "\n"))  [nil [(vec "\n") 0]]))
    (is (= (jb/run jp/skip-newline (cs/create "\r\n"))  [nil [(vec "\r\n") 1]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/skip-newline (cs/create " ")))))
  (testing "space"
    (is (= (jb/run jp/space (cs/create " "))  [\space [(vec " ") 0]]))
    (is (= (jb/run jp/space (cs/create "\t"))  [\tab [(vec "\t") 0]]))
    (is (= (jb/run jp/space (cs/create "\f"))  [\formfeed [(vec "\f") 0]]))
    (is (= (jb/run jp/space (cs/create "\r"))  [\newline [(vec "\r") 0]]))
    (is (= (jb/run jp/space (cs/create "\n"))  [\newline [(vec "\n") 0]]))
    (is (= (jb/run jp/space (cs/create "\r\n"))  [\newline [(vec "\r\n") 1]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/space (cs/create "A")))))
  (testing "skip-space"
    (is (= (jb/run jp/skip-space (cs/create " "))  [nil [(vec " ") 0]]))
    (is (= (jb/run jp/skip-space (cs/create "\t"))  [nil [(vec "\t") 0]]))
    (is (= (jb/run jp/skip-space (cs/create "\f"))  [nil [(vec "\f") 0]]))
    (is (= (jb/run jp/skip-space (cs/create "\r"))  [nil [(vec "\r") 0]]))
    (is (= (jb/run jp/skip-space (cs/create "\n"))  [nil [(vec "\n") 0]]))
    (is (= (jb/run jp/skip-space (cs/create "\r\n"))  [nil [(vec "\r\n") 1]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/skip-space (cs/create "A")))))
  (testing "spaces*"
    (is (= (jb/run jp/spaces* (cs/create "\r\na"))  [[\newline] [(vec "\r\na") 1]]))
    (is (= (jb/run jp/spaces* (cs/create "\r\n\t \fa"))  [[\newline \tab \space \formfeed] [(vec "\r\n\t \fa") 4]]))
    (is (= (jb/run jp/spaces* (cs/create "abcde"))  [nil [(vec "abcde") -1]])))
  (testing "skip-spaces*"
    (is (= (jb/run jp/skip-spaces* (cs/create "\r\na"))  [nil [(vec "\r\na") 1]]))
    (is (= (jb/run jp/skip-spaces* (cs/create "\r\n\t \fa"))  [nil [(vec "\r\n\t \fa") 4]]))
    (is (= (jb/run jp/skip-spaces* (cs/create "abcde"))  [nil [(vec "abcde") -1]])))
  (testing "spaces+"
    (is (= (jb/run jp/spaces+ (cs/create "\ra"))  [[\newline] [(vec "\ra") 0]]))
    (is (= (jb/run jp/spaces+ (cs/create "\r\n\t \fa"))  [[\newline \tab \space \formfeed] [(vec "\r\n\t \fa") 4]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/spaces+ (cs/create "abcde")))))
  (testing "skip-spaces+"
    (is (= (jb/run jp/skip-spaces+ (cs/create "\ra"))  [nil [(vec "\ra") 0]]))
    (is (= (jb/run jp/skip-spaces+ (cs/create "\r\n\t \fa"))  [nil [(vec "\r\n\t \fa") 4]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/skip-spaces+ (cs/create "abcde")))))
  (testing "eof"
    (is (= (jb/run jp/eof (cs/create ""))  [nil [(vec "") -1]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run jp/eof (cs/create "A")))))
  (testing "string"
    (is (= (jb/run (jp/string "abcd\t") (cs/create "abcd\tf")) ["abcd\t" [(vec "abcd\tf") 4]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/string "lt") (cs/create "abcd\tf")))))
  (testing "skip-string"
    (is (= (jb/run (jp/skip-string "abcd\t") (cs/create "abcd\tf")) [nil [(vec "abcd\tf") 4]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ParseError" (jb/run (jp/skip-string "lt") (cs/create "abcd\tf"))))))
