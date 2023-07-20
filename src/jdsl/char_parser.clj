(ns jdsl.char-parser
  "`jdsl.char-parser` contains definitions of basic char/string parsers."
  (:refer-clojure :exclude [char newline read read-line])
  (:require [jdsl.char-stream :as cs]
            [jdsl.basic       :as jb]
            [jdsl.combinator  :as jc]
            [jdsl.util        :as util]))

(defn char
  "Parses a char `c`, (\\r\\n, \\r, \\n) to \\newline."
  [c]
  (fn [cs]
    (cs/read-char cs c)))

(defn skip-char
  "Skips the char `c`"
  [c]
  (fn [cs]
    (when-let [[_ cs] (cs/read-char cs c)]
      (jb/ok nil cs))))

(def any-char
  "Parses any char, taking care of newline variants."
  (fn [cs]
    (when-let [[char cs] (cs/read cs)]
      (jb/ok char cs))))

(def skip-any-char
  "Skips any char, taking care of newline variants."
  (fn [cs]
    (when-let [[_ cs] (cs/read cs)]
      (jb/ok nil cs))))

(defn satisfy
  "`satisfy f` parses any char or newline for which
   the predicate function `f` returns true."
  [pred]
  (fn [cs]
    (when-let [[char cs] (cs/read cs)]
    (when (pred char)
      (jb/ok char cs)))))

(defn skip-satisfy
  "`skip-satisfy f` skips any char or newline for which  
   the predicate function `f` returns true."
  [pred]
  (fn [cs]
    (when-let [[char cs] (cs/read cs)]
    (when (pred char)
      (jb/ok nil cs)))))

(defn any-of
  "Parses any char contained in the specified `string`."
  [string]
  (satisfy (set string)))

(defn skip-any-of
  "Skips any char contained in the specified `string`."
  [string]
  (skip-satisfy (set string)))

(defn none-of
  "Parses any char NOT contained in the specified `string`."
  [string]
  (satisfy (comp not (set string))))

(defn skip-none-of
  "Skips any char NOT contained in the specified `string`"
  [string]
  (skip-satisfy (comp not (set string))))

(def lower
  "Parses a lower case character."
  (satisfy util/lower?))

(def upper
  "Parses a upper case character."
  (satisfy util/upper?))

(def alphabet
  "Parses alphabets. a-z or A-Z"
  (satisfy util/alphabet?))

(def digit
  "Parses a digit (0-9)."
  (satisfy util/digit?))

(def alpha-num
  "Parser alpha-numeric character. a-z or A-Z or 0-9"
  (satisfy util/alpha-num?))

(def hex
  "Parses a hex character."
  (satisfy util/hex?))

(def octal
  "Parses a octal character."
  (satisfy util/octal?))

(def tab
  "Parses a tab character."
  (char \tab))

(def newline
  "Parses a newline character."
  (char \newline))

(def skip-newline
  "Skips a newline character."
  (skip-char \newline))

(def space
  "Parses a whitespace character like \\newline, \\space, \\return, \\tab, \\formfeed"
  (satisfy util/space?))

(def spaces*
  "Parses zero or more whitespace characters."
  (jc/many* space))

(def spaces+
  "Parses one or more whitespace characters."
  (jc/many+ space))

(def skip-space
  "Skips a whitespace character like \\newline, \\space, \\return, \\tab, \\formfeed"
  (skip-satisfy util/space?))

(def skip-spaces*
  "Skips zero or more whitespace characters."
  (jc/skip-many* space))

(def skip-spaces+
  "Skips one or more whitespace characters."
  (jc/skip-many+ space))

(def eof
  "`eof` parser succeeds only at the end of the input."
  (fn [cs]
    (when-not (cs/peek cs)
      (jb/ok nil cs))))

(defn string
  "Parses the `string`"
  [string]
  (fn [cs]
    (loop [str string
           cs  cs]
      (if (empty? str)
        (jb/ok string cs)
      (when-let [[_ cs] (cs/read-char cs (first str))]
        (recur (next str) cs))))))

(defn skip-string
  "Skips the `string`"
  [string]
  (fn [cs]
    (loop [str string
           cs  cs]
      (if (empty? str)
        (jb/ok nil cs)
      (when-let [[_ cs] (cs/read-char cs (first str))]
        (recur (next str) cs))))))

(defn read
  "Reads n characters and returns a vec of characters"
  [n]
  (fn [cs]
    (cs/read cs n)))

(def read-line
  "Reads the whole line. Returns string."
  (fn [cs]
    (loop [cs  cs
          acc []]
      (let [[c ncs :as p] (cs/read cs)]
      (if (or (jb/EOS? p) (= c \newline))
        (-> [(apply str acc) cs])
      (recur ncs (conj acc c)))))))

(def word
  "Reads until the next whitespace character and returns string."
  (fn [cs]
    (loop [cs  cs
           acc []]
      (let [[c ncs] (cs/read cs)]
      (if (or (jb/EOS? c) (util/space? c))
        (-> [(apply str acc) cs])
      (recur ncs (conj acc c)))))))
