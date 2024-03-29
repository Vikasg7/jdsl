(ns jdsl.char-parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [jdsl.test-util :refer [is-parse-error?]]
            [jdsl.basic :as jb]
            [jdsl.char-stream :as cs]
            [jdsl.char-parser :as jp]))

(deftest char-parser-test
  (testing "char"
    (is (= (jb/run (jp/char \a) (cs/create "abc"))  [\a [(vec "abc") 0 3]]))
    (is (= (jb/run (jp/char \newline) (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/char \return) (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/char \return) (cs/create "\n")) [\newline [(vec "\n") 0 1]]))
    (is (= (jb/run (jp/char \newline) (cs/create "\r")) [\newline [(vec "\r") 0 1]]))
    (is-parse-error? (jb/run (jp/char \a) (cs/create "\r\n"))))
  (testing "skip-char"
    (is (= (jb/run (jp/skip-char \a) (cs/create "abc"))  [nil [(vec "abc") 0 3]]))
    (is (= (jb/run (jp/skip-char \newline) (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/skip-char \return) (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/skip-char \return) (cs/create "\n")) [nil [(vec "\n") 0 1]]))
    (is (= (jb/run (jp/skip-char \newline) (cs/create "\r")) [nil [(vec "\r") 0 1]]))
    (is-parse-error? (jb/run (jp/skip-char \a) (cs/create "\r\n"))))
  (testing "any-char"
    (is (= (jb/run jp/any-char (cs/create "abc"))  [\a [(vec "abc") 0 3]]))
    (is (= (jb/run jp/any-char (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run jp/any-char (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run jp/any-char (cs/create "\n")) [\newline [(vec "\n") 0 1]]))
    (is (= (jb/run jp/any-char (cs/create "\r")) [\newline [(vec "\r") 0 1]])))
  (testing "skip-any-char"
    (is (= (jb/run jp/skip-any-char (cs/create "abc"))  [nil [(vec "abc") 0 3]]))
    (is (= (jb/run jp/skip-any-char (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run jp/skip-any-char (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run jp/skip-any-char (cs/create "\n")) [nil [(vec "\n") 0 1]]))
    (is (= (jb/run jp/skip-any-char (cs/create "\r")) [nil [(vec "\r") 0 1]])))
  (testing "satisfy"
    (is (= (jb/run (jp/satisfy (partial = \a)) (cs/create "abc"))  [\a [(vec "abc") 0 3]]))
    (is (= (jb/run (jp/satisfy (partial = \newline)) (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/satisfy (partial = \newline)) (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/satisfy (partial = \newline)) (cs/create "\n")) [\newline [(vec "\n") 0 1]]))
    (is (= (jb/run (jp/satisfy (partial = \newline)) (cs/create "\r")) [\newline [(vec "\r") 0 1]]))
    (is-parse-error? (jb/run (jp/satisfy (partial = \a)) (cs/create "\r\n"))))
  (testing "skip-satisfy"
    (is (= (jb/run (jp/skip-satisfy (partial = \a)) (cs/create "abc"))  [nil [(vec "abc") 0 3]]))
    (is (= (jb/run (jp/skip-satisfy (partial = \newline)) (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/skip-satisfy (partial = \newline)) (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/skip-satisfy (partial = \newline)) (cs/create "\n")) [nil [(vec "\n") 0 1]]))
    (is (= (jb/run (jp/skip-satisfy (partial = \newline)) (cs/create "\r")) [nil [(vec "\r") 0 1]]))
    (is-parse-error? (jb/run (jp/skip-satisfy (partial = \a)) (cs/create "\r\n"))))
  (testing "any-of"
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "abc"))  [\a [(vec "abc") 0 3]]))
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "\n")) [\newline [(vec "\n") 0 1]]))
    (is (= (jb/run (jp/any-of "a\r\n") (cs/create "\r")) [\newline [(vec "\r") 0 1]]))
    (is-parse-error? (jb/run (jp/any-of "a\r\n") (cs/create "bcd"))))
  (testing "skip-any-of"
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "abc"))  [nil [(vec "abc") 0 3]]))
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "\n")) [nil [(vec "\n") 0 1]]))
    (is (= (jb/run (jp/skip-any-of "a\r\n") (cs/create "\r")) [nil [(vec "\r") 0 1]]))
    (is-parse-error? (jb/run (jp/skip-any-of "a\r\n") (cs/create "bcd"))))
  (testing "none-of"
    (is (= (jb/run (jp/none-of "bcd") (cs/create "abc"))  [\a [(vec "abc") 0 3]]))
    (is (= (jb/run (jp/none-of "bcd") (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/none-of "bcd") (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/none-of "bcd") (cs/create "\n")) [\newline [(vec "\n") 0 1]]))
    (is (= (jb/run (jp/none-of "bcd") (cs/create "\r")) [\newline [(vec "\r") 0 1]]))
    (is-parse-error? (jb/run (jp/none-of "a\r\n") (cs/create "abc"))))
  (testing "skip-none-of"
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "abc"))  [nil [(vec "abc") 0 3]]))
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "\r\n")) [nil [(vec "\r\n") 1 2]]))
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "\n")) [nil [(vec "\n") 0 1]]))
    (is (= (jb/run (jp/skip-none-of "bcd") (cs/create "\r")) [nil [(vec "\r") 0 1]]))
    (is-parse-error? (jb/run (jp/skip-none-of "a\r\n") (cs/create "abc"))))
  (testing "lower"
    (is (= (jb/run jp/lower (cs/create "a"))  [\a [(vec "a") 0 1]]))
    (is (= (jb/run jp/lower (cs/create "z")) [\z [(vec "z") 0 1]]))
    (is-parse-error? (jb/run jp/lower (cs/create "A"))))
  (testing "upper"
    (is (= (jb/run jp/upper (cs/create "A"))  [\A [(vec "A") 0 1]]))
    (is (= (jb/run jp/upper (cs/create "Z")) [\Z [(vec "Z") 0 1]]))
    (is-parse-error? (jb/run jp/upper (cs/create "a"))))
  (testing "alphabet"
    (is (= (jb/run jp/alphabet (cs/create "A"))  [\A [(vec "A") 0 1]]))
    (is (= (jb/run jp/alphabet (cs/create "Z")) [\Z [(vec "Z") 0 1]]))
    (is (= (jb/run jp/alphabet (cs/create "a"))  [\a [(vec "a") 0 1]]))
    (is (= (jb/run jp/alphabet (cs/create "z")) [\z [(vec "z") 0 1]]))
    (is-parse-error? (jb/run jp/alphabet (cs/create "0"))))
  (testing "alphabet"
    (is (= (jb/run jp/alpha-num (cs/create "A"))  [\A [(vec "A") 0 1]]))
    (is (= (jb/run jp/alpha-num (cs/create "Z")) [\Z [(vec "Z") 0 1]]))
    (is (= (jb/run jp/alpha-num (cs/create "a"))  [\a [(vec "a") 0 1]]))
    (is (= (jb/run jp/alpha-num (cs/create "z")) [\z [(vec "z") 0 1]]))
    (is (= (jb/run jp/alpha-num (cs/create "0"))  [\0 [(vec "0") 0 1]]))
    (is (= (jb/run jp/alpha-num (cs/create "9")) [\9 [(vec "9") 0 1]]))
    (is-parse-error? (jb/run jp/alpha-num (cs/create ","))))
  (testing "digit"
    (is (= (jb/run jp/digit (cs/create "0"))  [\0 [(vec "0") 0 1]]))
    (is (= (jb/run jp/digit (cs/create "9")) [\9 [(vec "9") 0 1]]))
    (is-parse-error? (jb/run jp/digit (cs/create "A"))))
  (testing "hex"
    (is (= (jb/run jp/hex (cs/create "0"))  [\0 [(vec "0") 0 1]]))
    (is (= (jb/run jp/hex (cs/create "9")) [\9 [(vec "9") 0 1]]))
    (is (= (jb/run jp/hex (cs/create "a")) [\a [(vec "a") 0 1]]))
    (is (= (jb/run jp/hex (cs/create "f")) [\f [(vec "f") 0 1]]))
    (is (= (jb/run jp/hex (cs/create "A")) [\A [(vec "A") 0 1]]))
    (is (= (jb/run jp/hex (cs/create "F")) [\F [(vec "F") 0 1]]))
    (is-parse-error? (jb/run jp/hex (cs/create "G")))
    (is-parse-error? (jb/run jp/hex (cs/create "g"))))
  (testing "octal"
    (is (= (jb/run jp/octal (cs/create "0"))  [\0 [(vec "0") 0 1]]))
    (is (= (jb/run jp/octal (cs/create "7")) [\7 [(vec "7") 0 1]]))
    (is-parse-error? (jb/run jp/octal (cs/create "8"))))
  (testing "tab"
    (is (= (jb/run jp/tab (cs/create "\t"))  [\tab [(vec "\t") 0 1]]))
    (is-parse-error? (jb/run jp/tab (cs/create " "))))
  (testing "newline"
    (is (= (jb/run jp/newline (cs/create "\n"))  [\newline [(vec "\n") 0 1]]))
    (is (= (jb/run jp/newline (cs/create "\r\n"))  [\newline [(vec "\r\n") 1 2]]))
    (is-parse-error? (jb/run jp/newline (cs/create " "))))
  (testing "skip-newline"
    (is (= (jb/run jp/skip-newline (cs/create "\n"))  [nil [(vec "\n") 0 1]]))
    (is (= (jb/run jp/skip-newline (cs/create "\r\n"))  [nil [(vec "\r\n") 1 2]]))
    (is-parse-error? (jb/run jp/skip-newline (cs/create " "))))
  (testing "space"
    (is (= (jb/run jp/space (cs/create " "))  [\space [(vec " ") 0 1]]))
    (is (= (jb/run jp/space (cs/create "\t"))  [\tab [(vec "\t") 0 1]]))
    (is (= (jb/run jp/space (cs/create "\f"))  [\formfeed [(vec "\f") 0 1]]))
    (is (= (jb/run jp/space (cs/create "\r"))  [\newline [(vec "\r") 0 1]]))
    (is (= (jb/run jp/space (cs/create "\n"))  [\newline [(vec "\n") 0 1]]))
    (is (= (jb/run jp/space (cs/create "\r\n"))  [\newline [(vec "\r\n") 1 2]]))
    (is-parse-error? (jb/run jp/space (cs/create "A"))))
  (testing "skip-space"
    (is (= (jb/run jp/skip-space (cs/create " "))  [nil [(vec " ") 0 1]]))
    (is (= (jb/run jp/skip-space (cs/create "\t"))  [nil [(vec "\t") 0 1]]))
    (is (= (jb/run jp/skip-space (cs/create "\f"))  [nil [(vec "\f") 0 1]]))
    (is (= (jb/run jp/skip-space (cs/create "\r"))  [nil [(vec "\r") 0 1]]))
    (is (= (jb/run jp/skip-space (cs/create "\n"))  [nil [(vec "\n") 0 1]]))
    (is (= (jb/run jp/skip-space (cs/create "\r\n"))  [nil [(vec "\r\n") 1 2]]))
    (is-parse-error? (jb/run jp/skip-space (cs/create "A"))))
  (testing "spaces*"
    (is (= (jb/run jp/spaces* (cs/create "\r\na"))  [[\newline] [(vec "\r\na") 1 3]]))
    (is (= (jb/run jp/spaces* (cs/create "\r\n\t \fa"))  [[\newline \tab \space \formfeed] [(vec "\r\n\t \fa") 4 6]]))
    (is (= (jb/run jp/spaces* (cs/create "abcde"))  [nil [(vec "abcde") -1 5]])))
  (testing "skip-spaces*"
    (is (= (jb/run jp/skip-spaces* (cs/create "\r\na"))  [nil [(vec "\r\na") 1 3]]))
    (is (= (jb/run jp/skip-spaces* (cs/create "\r\n\t \fa"))  [nil [(vec "\r\n\t \fa") 4 6]]))
    (is (= (jb/run jp/skip-spaces* (cs/create "abcde"))  [nil [(vec "abcde") -1 5]])))
  (testing "spaces+"
    (is (= (jb/run jp/spaces+ (cs/create "\ra"))  [[\newline] [(vec "\ra") 0 2]]))
    (is (= (jb/run jp/spaces+ (cs/create "\r\n\t \fa"))  [[\newline \tab \space \formfeed] [(vec "\r\n\t \fa") 4 6]]))
    (is-parse-error? (jb/run jp/spaces+ (cs/create "abcde"))))
  (testing "skip-spaces+"
    (is (= (jb/run jp/skip-spaces+ (cs/create "\ra"))  [nil [(vec "\ra") 0 2]]))
    (is (= (jb/run jp/skip-spaces+ (cs/create "\r\n\t \fa"))  [nil [(vec "\r\n\t \fa") 4 6]]))
    (is-parse-error? (jb/run jp/skip-spaces+ (cs/create "abcde"))))
  (testing "eof"
    (is (= (jb/run jp/eof (cs/create ""))  [nil [nil -1 0]]))
    (is-parse-error? (jb/run jp/eof (cs/create "A"))))
  (testing "string"
    (is (= (jb/run (jp/string "abcd\t") (cs/create "abcd\tf")) ["abcd\t" [(vec "abcd\tf") 4 6]]))
    (is-parse-error? (jb/run (jp/string "lt") (cs/create "abcd\tf"))))
  (testing "skip-string"
    (is (= (jb/run (jp/skip-string "abcd\t") (cs/create "abcd\tf")) [nil [(vec "abcd\tf") 4 6]]))
    (is-parse-error? (jb/run (jp/skip-string "lt") (cs/create "abcd\tf"))))
  (testing "read"
    (is (= (jb/run (jp/read 1) (cs/create "abcd\tf")) [\a [(vec "abcd\tf") 0 6]]))
    (is (= (jb/run (jp/read 2) (cs/create "abcd\tf")) [[\a \b] [(vec "abcd\tf") 1 6]]))
    (is (= (jb/run (jp/read 0) (cs/create "abcd\tf")) [nil [(vec "abcd\tf") -1 6]]))
    (is (= (jb/run (jp/read 8) (cs/create "abcd\tf")) [(vec "abcd\tf") [(vec "abcd\tf") 5 6]])))
  (testing "read-line"
    (is (= (jb/run jp/read-line (cs/create "abcd\nf")) ["abcd" [(vec "abcd\nf") 3 6]]))
    (is (= (jb/run jp/read-line (cs/create "abcd\r\nf")) ["abcd" [(vec "abcd\r\nf") 3 7]]))
    (is (= (jb/run jp/read-line (cs/create "\nf")) ["" [(vec "\nf") -1 2]]))
    (is (= (jb/run jp/read-line (cs/create "abcdf")) ["abcdf" [(vec "abcdf") 4 5]])))
  (testing "word"
    (is (= (jb/run jp/word (cs/create "abcd f")) ["abcd" [(vec "abcd f") 3 6]]))
    (is (= (jb/run jp/word (cs/create "\nf")) ["" [(vec "\nf") -1 2]]))
    (is (= (jb/run jp/word (cs/create "abcdf")) ["abcdf" [(vec "abcdf") 4 5]]))))
