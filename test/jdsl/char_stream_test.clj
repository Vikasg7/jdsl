(ns jdsl.char-stream-test
  (:require [clojure.test :refer [deftest testing is]]
            [jdsl.basic :as jb]
            [jdsl.char-stream :as cs]))

(deftest char-stream-test
  (testing "create"
    (is (= (cs/create "abc") [(vec "abc") -1 3]))
    (is (= (cs/create "") [nil -1 0]))
    (is (= (cs/create nil) [nil -1 0])))
  (testing "buf pos end"
    (is (= (cs/buf (cs/create "abc")) (vec "abc")))
    (is (= (cs/pos (cs/create "abc")) -1))
    (is (= (cs/end (cs/create "abc")) 3)))
  (testing "next"
    (is (= (cs/next (cs/create "abc")) [\a [(vec "abc") 0 3]]))
    (is (= (cs/next (cs/create "")) nil)))
  (testing "eof"
    (is (= (jb/run cs/state (cs/create "abc"))  [[(vec "abc") -1 3] [(vec "abc") -1 3]])))
  (testing "peek/1"
    (is (= (cs/peek (cs/create "abc")) \a))
    (is (= (cs/peek (cs/create "")) nil)))
  (testing "peek/2"
    (is (= (cs/peek (cs/create "abc") 2) [\a \b]))
    (is (= (cs/peek (cs/create "abc") 5) [\a \b \c]))
    (is (= (cs/peek (cs/create "") 2) nil)))
  (testing "peek-nth"
    (is (= (cs/peek-nth (cs/create "abc") 2) \b))
    (is (= (cs/peek-nth (cs/create "") 1) nil))
    (is (= (cs/peek-nth (cs/create "abc") 6) nil)))
  (testing "match"
    (is (= (cs/match (cs/create "abc") \a) true))
    (is (= (cs/match (cs/create "abc") \b) false))
    (is (= (cs/match (cs/create "") \a) false)))
  (testing "match-seq"
    (is (= (cs/match-seq (cs/create "abc") [\a \b \c]) true))
    (is (= (cs/match-seq (cs/create "abc") [\c]) false))
    (is (= (cs/match-seq (cs/create "") nil) true)))
  (testing "match-str"
    (is (= (cs/match-str (cs/create "abc") "abc") true))
    (is (= (cs/match-str (cs/create "abc") "c") false))
    (is (= (cs/match-str (cs/create "") "") true)))
  (testing "skip/1"
    (is (= (cs/skip (cs/create "abc")) [(vec "abc") 0 3]))
    (is (= (cs/skip (cs/create "")) nil)))
  (testing "skip/2"
    (is (= (cs/skip (cs/create "abc") 2) [(vec "abc") 1 3]))
    (is (= (cs/skip (cs/create "abc") 5) nil))
    (is (= (cs/skip (cs/create "") 1) nil)))
  (testing "skip-newline"
    (is (= (cs/skip-newline (cs/create "\r\n")) [(vec "\r\n") 1 2]))
    (is (= (cs/skip-newline (cs/create "\r")) [(vec "\r") 0 1]))
    (is (= (cs/skip-newline (cs/create "\n")) [(vec "\n") 0 1]))
    (is (= (cs/skip-newline (cs/create "")) nil)))
  (testing "skip-char"
    (is (= (cs/skip-char (cs/create "abc") \a) [(vec "abc") 0 3]))
    (is (= (cs/skip-char (cs/create "abc") \b) nil))
    (is (= (cs/skip-char (cs/create "") \a) nil)))
  (testing "read/1"
    (is (= (cs/read (cs/create "abc")) [\a [(vec "abc") 0 3]]))
    (is (= (cs/read (cs/create "\r\n")) [\newline [(vec "\r\n") 1 2]]))
    (is (= (cs/read (cs/create "\r")) [\newline [(vec "\r") 0 1]]))
    (is (= (cs/read (cs/create "\n")) [\newline [(vec "\n") 0 1]]))
    (is (= (cs/read (cs/create "")) nil)))
  (testing "read/2"
    (is (= (cs/read (cs/create "abc") 1) [\a [(vec "abc") 0 3]]))
    (is (= (cs/read (cs/create "abc") 2) [[\a \b] [(vec "abc") 1 3]]))
    (is (= (cs/read (cs/create "\r\n") 1) [\newline [(vec "\r\n") 1 2]]))
    (is (= (cs/read (cs/create "\r") 1) [\newline [(vec "\r") 0 1]]))
    (is (= (cs/read (cs/create "\n") 1) [\newline [(vec "\n") 0 1]]))
    (is (= (cs/read (cs/create "") 1) [nil [nil -1 0]])))
  (testing "read-char"
    (is (= (cs/read-char (cs/create "abc") \a) [\a [(vec "abc") 0 3]]))
    (is (= (cs/read-char (cs/create "\r\n") \newline) [\newline [(vec "\r\n") 1 2]]))
    (is (= (cs/read-char (cs/create "\r") \newline) [\newline [(vec "\r") 0 1]]))
    (is (= (cs/read-char (cs/create "\n") \return) [\newline [(vec "\n") 0 1]]))
    (is (= (cs/read-char (cs/create "") \a) nil))))
