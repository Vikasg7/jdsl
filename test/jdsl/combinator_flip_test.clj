(ns jdsl.combinator-flip-test
  (:require [clojure.test :refer [deftest testing is]]
            [jdsl.basic :as jb]
            [jdsl.char-stream :as cs]
            [jdsl.char-parser :as jp]
            [jdsl.combinator :as jc]
            [jdsl.combinator-flip :as jf]))

(deftest flipped-test
  (testing "<$>"
    (is (= (jb/run (jf/<$> (jp/char \a) str) (cs/create "abc")) ["a" [(vec "abc") 0]])))
  (testing "<$"
    (is (= (jb/run (jf/<$ (jp/char \a) "a") (cs/create "abc")) ["a" [(vec "abc") 0]])))
  (testing "$>"
    (is (= (jb/run (jf/$> "a" (jp/char \a)) (cs/create "abc")) ["a" [(vec "abc") 0]])))
  (testing ">>="
    (is (= (jb/run (jf/>>= (fn [_] (jp/char \b)) (jp/char \a)) (cs/create "abc")) [\b [(vec "abc") 1]])))
  (testing "=<<"
    (is (= (jb/run (jf/=<< (jp/char \a) (fn [_] (jp/char \b))) (cs/create "abc")) [\b [(vec "abc") 1]])))
  (testing ">>"
    (is (= (jb/run (jf/>> (jp/char \b) (jp/char \a)) (cs/create "abc")) [\b [(vec "abc") 1]])))
  (testing "<<"
    (is (= (jb/run (jf/<< (jp/char \b) (jp/char \a)) (cs/create "abc")) [\a [(vec "abc") 1]])))
  (testing "<*>"
    (is (= (jb/run (jf/<*> (jp/char \a) (jc/return str)) (cs/create "abc")) ["a" [(vec "abc") 0]])))
  (testing "<?>"
    (try 
      (jb/run (jf/<?> "Expected: a" (jp/char \a)) (cs/create "bcd"))
    (catch clojure.lang.ExceptionInfo e
      (is (= "ParseError" (ex-message e)))
      (is (= "Expected: a" (:msg (ex-data e))))
      (is (= [(vec "bcd") -1] (:ts (ex-data e)))))))
  (testing "between"
    (is (= (jb/run (jf/between (jp/char \a) (jp/char \c) (jp/char \b)) (cs/create "abc")) [\b [(vec "abc") 2]]))))
