(ns jdsl.combinator-test
  (:require [clojure.test :refer [deftest testing is]]
            [jdsl.test-util :refer [is-parse-error?]]
            [jdsl.basic :as jb]
            [jdsl.char-stream :as cs]
            [jdsl.char-parser :as jp]
            [jdsl.combinator :as jc]))

(deftest combinator-test
  (testing "<$>"
    (is (= (jb/run (jc/<$> str (jp/char \a)) (cs/create "abc")) ["a" [(vec "abc") 0 3]]))
    (is-parse-error? (jb/run (jc/<$> #{\b} (jp/char \a)) (cs/create "abc"))))
  (testing "map"
    (is (= (jb/run (jc/map str (jp/char \a)) (cs/create "abc")) ["a" [(vec "abc") 0 3]]))
    (is-parse-error? (jb/run (jc/map #{\b} (jp/char \a)) (cs/create "abc"))))
  (testing "<$"
    (is (= (jb/run (jc/<$ "a" (jp/char \a)) (cs/create "abc")) ["a" [(vec "abc") 0 3]])))
  (testing "$>"
    (is (= (jb/run (jc/$> (jp/char \a) "a") (cs/create "abc")) ["a" [(vec "abc") 0 3]])))
  (testing "return"
    (is (= (jb/run (jc/return "a") (cs/create "abc")) ["a" [(vec "abc") -1 3]])))
  (testing "zero"
    (is (jb/error? (jc/zero (cs/create "abc"))))
    (is (= (jc/zero (cs/create "abc")) nil))
    (is-parse-error? (jb/run jc/zero (cs/create "abc"))))
  (testing "fail"
    (is (jb/error? ((jc/fail-with "Expected: a") (cs/create "abc"))))
    (is (= ((jc/fail-with "Expected: a") (cs/create "abc")) "Expected: a"))
    (is-parse-error? (jb/run (jc/fail-with "Exception: a") (cs/create "abc"))))
  (testing "lift"
    (is (= (jb/run ((jc/lift {:a 1}) :a) (cs/create "abc")) [1 [(vec "abc") -1 3]])))
  (testing ">>="
    (is (= (jb/run (jc/>>= (jp/char \a) (fn [_] (jp/char \b))) (cs/create "abc")) [\b [(vec "abc") 1 3]])))
  (testing "bind"
    (is (= (jb/run (jc/bind (jp/char \a) (fn [_] (jp/char \b))) (cs/create "abc")) [\b [(vec "abc") 1 3]])))
  (testing "=<<"
    (is (= (jb/run (jc/=<< (fn [_] (jp/char \b)) (jp/char \a)) (cs/create "abc")) [\b [(vec "abc") 1 3]])))
  (testing ">>"
    (is (= (jb/run (jc/>> (jp/char \a) (jp/char \b)) (cs/create "abc")) [\b [(vec "abc") 1 3]])))
  (testing "<<"
    (is (= (jb/run (jc/<< (jp/char \a) (jp/char \b)) (cs/create "abc")) [\a [(vec "abc") 1 3]])))
  (testing "<*>"
    (is (= (jb/run (jc/<*> (jc/return str) (jp/char \a)) (cs/create "abc")) ["a" [(vec "abc") 0 3]])))
  (testing "apply"
    (is (= (jb/run (jc/apply (jc/return str) (jp/char \a)) (cs/create "abc")) ["a" [(vec "abc") 0 3]])))
  (testing "<>"
    (is (= (jb/run (jc/<> (jp/char \a) (jp/char \b)) (cs/create "abc")) [[\a \b] [(vec "abc") 1 3]])))
  (testing "<?>"
    (try 
      (jb/run (jc/<?> (jp/char \a) "Expected: a") (cs/create "bcd"))
    (catch clojure.lang.ExceptionInfo e
      (is (= "ParseError" (ex-message e)))
      (is (= "Expected: a" (:msg (ex-data e))))
      (is (= [(vec "bcd") -1 3] (:ts (ex-data e)))))))
  (testing "label"
    (try 
      (jb/run (jc/label (jp/char \a) "Expected: a") (cs/create "bcd"))
    (catch clojure.lang.ExceptionInfo e
      (is (= "ParseError" (ex-message e)))
      (is (= "Expected: a" (:msg (ex-data e))))
      (is (= [(vec "bcd") -1 3] (:ts (ex-data e)))))))
  (testing "<|>"
    (is (= (jb/run (jc/<|> (jp/char \a) (jp/char \b)) (cs/create "bac")) [\b [(vec "bac") 0 3]])))
  (testing "alt"
    (is (= (jb/run (jc/alt (jp/char \a) (jp/char \b)) (cs/create "bac")) [\b [(vec "bac") 0 3]])))
  (testing "optional"
    (is (= (jb/run (jc/optional (jp/char \a)) (cs/create "bac")) [nil [(vec "bac") -1 3]]))
    (is (= (jb/run (jc/optional (jp/char \a)) (cs/create "abc")) [nil [(vec "abc") 0 3]])))
  (testing "skip"
    (is (= (jb/run (jc/skip (jp/char \a)) (cs/create "bac")) [nil [(vec "bac") -1 3]]))
    (is (= (jb/run (jc/skip (jp/char \a)) (cs/create "abc")) [nil [(vec "abc") 0 3]])))
  (testing "peek"
    (is (= (jb/run (jc/peek (jp/char \a)) (cs/create "bac")) [nil [(vec "bac") -1 3]]))
    (is (= (jb/run (jc/peek (jp/char \a)) (cs/create "abc")) [\a [(vec "abc") -1 3]])))
  (testing "between"
    (is (= (jb/run (jc/between (jp/char \b) (jp/char \a) (jp/char \c)) (cs/create "abc")) [\b [(vec "abc") 2 3]])))
  (testing "many*"
    (is (= (jb/run (jc/many* (jp/char \b)) (cs/create "abc")) [nil [(vec "abc") -1 3]]))
    (is (= (jb/run (jc/many* (jp/char \b)) (cs/create "bba")) [[\b \b] [(vec "bba") 1 3]])))
  (testing "many+"
    (is (= (jb/run (jc/many+ (jp/char \b)) (cs/create "bba")) [[\b \b] [(vec "bba") 1 3]]))
    (is-parse-error? (jb/run (jc/many+ (jp/char \b)) (cs/create "abc"))))
  (testing "choice"
    (is (= (jb/run (jc/choice [(jp/char \b) (jp/char \a)]) (cs/create "abc")) [\a [(vec "abc") 0 3]]))
    (is-parse-error? (jb/run (jc/choice [(jp/char \b) (jp/char \c)]) (cs/create "abc"))))
  (testing "followed-by"
    (is (= (jb/run (jc/<< (jp/char \b) (jc/followed-by (jp/char \a))) (cs/create "bac")) [\b [(vec "bac") 0 3]]))
    (is-parse-error? (jb/run (jc/<< (jp/char \b) (jc/followed-by (jp/char \a))) (cs/create "abc"))))
  (testing "not-followed-by"
    (is (= (jb/run (jc/<< (jp/char \b) (jc/not-followed-by (jp/char \a))) (cs/create "bca")) [\b [(vec "bca") 0 3]]))
    (is-parse-error? (jb/run (jc/<< (jp/char \b) (jc/not-followed-by (jp/char \a))) (cs/create "bac"))))
  (testing "skip-many*"
    (is (= (jb/run (jc/skip-many* (jp/char \b)) (cs/create "abc")) [nil [(vec "abc") -1 3]]))
    (is (= (jb/run (jc/skip-many* (jp/char \b)) (cs/create "bba")) [nil [(vec "bba") 1 3]])))
  (testing "skip-many+"
    (is (= (jb/run (jc/skip-many+ (jp/char \b)) (cs/create "bba")) [nil [(vec "bba") 1 3]]))
    (is-parse-error? (jb/run (jc/skip-many+ (jp/char \b)) (cs/create "abc"))))
  (testing "sep-by*"
    (is (= (jb/run (jc/sep-by* jp/any-char (jp/char \,)) (cs/create "a,bc")) [[\a \b] [(vec "a,bc") 2 4]]))
    (is (= (jb/run (jc/sep-by* jp/any-char (jp/char \,)) (cs/create "ac")) [[\a] [(vec "ac") 0 2]]))
    (is (= (jb/run (jc/sep-by* (jp/char \b) (jp/char \,)) (cs/create "aba")) [nil [(vec "aba") -1 3]]))
    (is-parse-error? (jb/run (jc/sep-by* jp/any-char (jp/char \,)) (cs/create "a,b,"))))
  (testing "sep-by+"
    (is (= (jb/run (jc/sep-by+ jp/any-char (jp/char \,)) (cs/create "a,bc")) [[\a \b] [(vec "a,bc") 2 4]]))
    (is (= (jb/run (jc/sep-by+ jp/any-char (jp/char \,)) (cs/create "ac")) [[\a] [(vec "ac") 0 2]]))
    (is-parse-error? (jb/run (jc/sep-by+ (jp/char \b) (jp/char \,)) (cs/create "a,bc")))
    (is-parse-error? (jb/run (jc/sep-by* jp/any-char (jp/char \,)) (cs/create "a,b,"))))
  (testing "skip-sep-by*"
    (is (= (jb/run (jc/skip-sep-by* jp/any-char (jp/char \,)) (cs/create "a,bc")) [nil [(vec "a,bc") 2 4]]))
    (is (= (jb/run (jc/skip-sep-by* jp/any-char (jp/char \,)) (cs/create "ac")) [nil [(vec "ac") 0 2]]))
    (is (= (jb/run (jc/skip-sep-by* (jp/char \b) (jp/char \,)) (cs/create "aba")) [nil [(vec "aba") -1 3]]))
    (is-parse-error? (jb/run (jc/sep-by* jp/any-char (jp/char \,)) (cs/create "a,b,"))))
  (testing "skip-sep-by+"
    (is (= (jb/run (jc/skip-sep-by+ jp/any-char (jp/char \,)) (cs/create "a,bc")) [nil [(vec "a,bc") 2 4]]))
    (is (= (jb/run (jc/skip-sep-by+ jp/any-char (jp/char \,)) (cs/create "ac")) [nil [(vec "ac") 0 2]]))
    (is-parse-error? (jb/run (jc/skip-sep-by+ (jp/char \b) (jp/char \,)) (cs/create "a,bc")))
    (is-parse-error? (jb/run (jc/sep-by* jp/any-char (jp/char \,)) (cs/create "a,b,"))))
  (testing "sep-end-by*"
    (is (= (jb/run (jc/sep-end-by* jp/any-char (jp/char \,)) (cs/create "a,b,c")) [[\a \b \c] [(vec "a,b,c") 4 5]]))
    (is (= (jb/run (jc/sep-end-by* jp/any-char (jp/char \,)) (cs/create "a,c")) [[\a \c] [(vec "a,c") 2 3]]))
    (is (= (jb/run (jc/sep-end-by* jp/any-char (jp/char \,)) (cs/create "a,bc")) [[\a \b] [(vec "a,bc") 2 4]]))
    (is (= (jb/run (jc/sep-end-by* jp/any-char (jp/char \,)) (cs/create "ac")) [[\a] [(vec "ac") 0 2]]))
    (is (= (jb/run (jc/sep-end-by* jp/any-char (jp/char \,)) (cs/create "a,b,")) [[\a \b] [(vec "a,b,") 3 4]]))
    (is (= (jb/run (jc/sep-end-by* (jp/char \b) (jp/char \,)) (cs/create "aba")) [nil [(vec "aba") -1 3]])))
  (testing "sep-end-by+"
    (is (= (jb/run (jc/sep-end-by+ jp/any-char (jp/char \,)) (cs/create "a,b,c")) [[\a \b \c] [(vec "a,b,c") 4 5]]))
    (is (= (jb/run (jc/sep-end-by+ jp/any-char (jp/char \,)) (cs/create "a,c")) [[\a \c] [(vec "a,c") 2 3]]))
    (is (= (jb/run (jc/sep-end-by+ jp/any-char (jp/char \,)) (cs/create "a,bc")) [[\a \b] [(vec "a,bc") 2 4]]))
    (is (= (jb/run (jc/sep-end-by+ jp/any-char (jp/char \,)) (cs/create "ac")) [[\a] [(vec "ac") 0 2]]))
    (is (= (jb/run (jc/sep-end-by+ jp/any-char (jp/char \,)) (cs/create "a,b,")) [[\a \b] [(vec "a,b,") 3 4]]))
    (is-parse-error? (jb/run (jc/sep-end-by+ (jp/char \b) (jp/char \,)) (cs/create "a,bc"))))
  (testing "skip-sep-end-by*"
    (is (= (jb/run (jc/skip-sep-end-by* jp/any-char (jp/char \,)) (cs/create "a,b,c")) [nil [(vec "a,b,c") 4 5]]))
    (is (= (jb/run (jc/skip-sep-end-by* jp/any-char (jp/char \,)) (cs/create "a,c")) [nil [(vec "a,c") 2 3]]))
    (is (= (jb/run (jc/skip-sep-end-by* jp/any-char (jp/char \,)) (cs/create "a,bc")) [nil [(vec "a,bc") 2 4]]))
    (is (= (jb/run (jc/skip-sep-end-by* jp/any-char (jp/char \,)) (cs/create "ac")) [nil [(vec "ac") 0 2]]))
    (is (= (jb/run (jc/skip-sep-end-by* jp/any-char (jp/char \,)) (cs/create "a,b,")) [nil [(vec "a,b,") 3 4]]))
    (is (= (jb/run (jc/skip-sep-end-by* (jp/char \b) (jp/char \,)) (cs/create "aba")) [nil [(vec "aba") -1 3]])))
  (testing "skip-sep-end-by+"
    (is (= (jb/run (jc/skip-sep-end-by+ jp/any-char (jp/char \,)) (cs/create "a,b,c")) [nil [(vec "a,b,c") 4 5]]))
    (is (= (jb/run (jc/skip-sep-end-by+ jp/any-char (jp/char \,)) (cs/create "a,c")) [nil [(vec "a,c") 2 3]]))
    (is (= (jb/run (jc/skip-sep-end-by+ jp/any-char (jp/char \,)) (cs/create "a,bc")) [nil [(vec "a,bc") 2 4]]))
    (is (= (jb/run (jc/skip-sep-end-by+ jp/any-char (jp/char \,)) (cs/create "ac")) [nil [(vec "ac") 0 2]]))
    (is (= (jb/run (jc/skip-sep-end-by+ jp/any-char (jp/char \,)) (cs/create "a,b,")) [nil [(vec "a,b,") 3 4]]))
    (is-parse-error? (jb/run (jc/skip-sep-end-by+ (jp/char \b) (jp/char \,)) (cs/create "a,bc"))))
  (testing "end-by*"
    (is (= (jb/run (jc/end-by* jp/any-char (jp/char \,)) (cs/create "a,b,c,")) [[\a \b \c] [(vec "a,b,c,") 5 6]]))
    (is (= (jb/run (jc/end-by* jp/any-char (jp/char \,)) (cs/create "a,c,")) [[\a \c] [(vec "a,c,") 3 4]]))
    (is-parse-error? (jb/run (jc/skip-end-by* jp/any-char (jp/char \,)) (cs/create "ac")))
    (is-parse-error? (jb/run (jc/skip-end-by* jp/any-char (jp/char \,)) (cs/create "a,b"))))
  (testing "end-by+"
    (is (= (jb/run (jc/end-by+ jp/any-char (jp/char \,)) (cs/create "a,b,c,")) [[\a \b \c] [(vec "a,b,c,") 5 6]]))
    (is (= (jb/run (jc/end-by+ jp/any-char (jp/char \,)) (cs/create "a,")) [[\a] [(vec "a,") 1 2]]))
    (is (= (jb/run (jc/end-by+ jp/any-char (jp/char \,)) (cs/create "a,b,")) [[\a \b] [(vec "a,b,") 3 4]]))
    (is-parse-error? (jb/run (jc/end-by+ (jp/char \b) (jp/char \,)) (cs/create "bbc")))
    (is-parse-error? (jb/run (jc/skip-end-by+ jp/any-char (jp/char \,)) (cs/create "a,c"))))
  (testing "skip-end-by*"
    (is (= (jb/run (jc/skip-end-by* jp/any-char (jp/char \,)) (cs/create "a,b,c,")) [nil [(vec "a,b,c,") 5 6]]))
    (is (= (jb/run (jc/skip-end-by* jp/any-char (jp/char \,)) (cs/create "a,c,")) [nil [(vec "a,c,") 3 4]]))
    (is-parse-error? (jb/run (jc/skip-end-by* jp/any-char (jp/char \,)) (cs/create "ac")))
    (is-parse-error? (jb/run (jc/skip-end-by* jp/any-char (jp/char \,)) (cs/create "a,b"))))
  (testing "skip-end-by+"
    (is (= (jb/run (jc/skip-end-by+ jp/any-char (jp/char \,)) (cs/create "a,b,c,")) [nil [(vec "a,b,c,") 5 6]]))
    (is (= (jb/run (jc/skip-end-by+ jp/any-char (jp/char \,)) (cs/create "a,")) [nil [(vec "a,") 1 2]]))
    (is (= (jb/run (jc/skip-end-by+ jp/any-char (jp/char \,)) (cs/create "a,b,")) [nil [(vec "a,b,") 3 4]]))
    (is-parse-error? (jb/run (jc/skip-end-by+ (jp/char \b) (jp/char \,)) (cs/create "bbc")))
    (is-parse-error? (jb/run (jc/skip-end-by+ jp/any-char (jp/char \,)) (cs/create "a,c"))))
  (testing "many-till*"
    (is (= (jb/run (jc/>> (jp/char \<) (jc/many-till* jp/any-char (jp/char \>))) (cs/create "<abc>")) [[\a \b \c] [(vec "<abc>") 4 5]]))
    (is (= (jb/run (jc/>> (jp/char \<) (jc/many-till* jp/any-char (jp/char \>))) (cs/create "<>")) [nil [(vec "<>") 1 2]]))
    (is-parse-error? (jb/run (jc/>> (jp/char \<) (jc/many-till* jp/any-char (jp/char \>))) (cs/create "<abc"))))
  (testing "many-till+"
    (is (= (jb/run (jc/>> (jp/char \<) (jc/many-till+ jp/any-char (jp/char \>))) (cs/create "<a>")) [[\a] [(vec "<a>") 2 3]]))
    (is (= (jb/run (jc/>> (jp/char \<) (jc/many-till+ jp/any-char (jp/char \>))) (cs/create "<abc>")) [[\a \b \c] [(vec "<abc>") 4 5]]))
    (is-parse-error? (jb/run (jc/>> (jp/char \<) (jc/many-till+ jp/any-char (jp/char \>))) (cs/create "<>")))
    (is-parse-error? (jb/run (jc/>> (jp/char \<) (jc/many-till+ jp/any-char (jp/char \>))) (cs/create "<abc"))))
  (testing "skip-many-till*"
    (is (= (jb/run (jc/>> (jp/char \<) (jc/skip-many-till* jp/any-char (jp/char \>))) (cs/create "<abc>")) [nil [(vec "<abc>") 4 5]]))
    (is (= (jb/run (jc/>> (jp/char \<) (jc/skip-many-till* jp/any-char (jp/char \>))) (cs/create "<>")) [nil [(vec "<>") 1 2]]))
    (is-parse-error? (jb/run (jc/>> (jp/char \<) (jc/skip-many-till* jp/any-char (jp/char \>))) (cs/create "<abc"))))
  (testing "skip-many-till+"
    (is (= (jb/run (jc/>> (jp/char \<) (jc/skip-many-till+ jp/any-char (jp/char \>))) (cs/create "<a>")) [nil [(vec "<a>") 2 3]]))
    (is (= (jb/run (jc/>> (jp/char \<) (jc/skip-many-till+ jp/any-char (jp/char \>))) (cs/create "<abc>")) [nil [(vec "<abc>") 4 5]]))
    (is-parse-error? (jb/run (jc/>> (jp/char \<) (jc/skip-many-till+ jp/any-char (jp/char \>))) (cs/create "<>")))
    (is-parse-error? (jb/run (jc/>> (jp/char \<) (jc/skip-many-till+ jp/any-char (jp/char \>))) (cs/create "<abc")))))
