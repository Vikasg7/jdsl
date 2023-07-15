(ns jdsl.char-parser
  "`jdsl.char-parser` contains definitons of basic char/string parsers."
  (:refer-clojure :exclude [char newline])
  (:require [jdsl.char-stream :as cs]
            [jdsl.basic :as jb]
            [jdsl.combinator :as jc]))

(defn char
  "Parses a char `c`, (\\r\\n, \\r, \\n) to \\newline."
  [c]
  (fn [cs]
    (cs/read cs c)))

(defn skip-char
  "Skips the char `c`"
  [c]
  (fn [cs]
    (when-let [[_ cs] (cs/read cs c)]
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
    (when-let [[char ts] (cs/read cs)]
    (when (pred char)
      (jb/ok char ts)))))

(defn skip-satisfy
  "`skip-satisfy f` skips any char or newline for which  
   the predicate function `f` returns true."
  [pred]
  (fn [cs]
    (when-let [[char ts] (cs/read cs)]
    (when (pred char)
      (jb/ok nil ts)))))

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
  (satisfy (fn lower? [c] (<= (int \a) (int c) (int \z)))))

(def upper
  "Parses a upper case character."
  (satisfy (fn upper? [c] (<= (int \A) (int c) (int \Z)))))

(def alphabet
  "Parses alphabets. a-z or A-Z"
  (satisfy (fn upper? [c] (or (<= (int \A) (int c) (int \Z))
                              (<= (int \a) (int c) (int \z))))))

(def digit
  "Parses a digit (0-9)."
  (satisfy (fn digit? [c] (<= (int \0) (int c) (int \9)))))

(def alpha-num
  "Parser alpha-numeric character. a-z or A-Z or 0-9"
  (satisfy (fn upper? [c] (or (<= (int \A) (int c) (int \Z))
                              (<= (int \a) (int c) (int \z))
                              (<= (int \0) (int c) (int \9))))))

(def hex
  "Parses a hex character."
  (satisfy (fn hex? [c]
             (or (<= (int \0) (int c) (int \9))
                 (<= (int \a) (int c) (int \f))
                 (<= (int \A) (int c) (int \F))))))

(def octal
  "Parses a octal character."
  (satisfy (fn octal? [c] (<= (int \0) (int c) (int \7)))))

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
  (satisfy (set "\r\n\t \f")))

(def spaces*
  "Parses zero or more whitespace characters."
  (jc/many* space))

(def spaces+
  "Parses one or more whitespace characters."
  (jc/many+ space))

(def skip-space
  "Skips a whitespace character like \\newline, \\space, \\return, \\tab, \\formfeed"
  (skip-satisfy (set "\r\n\t \f")))

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
      (when-let [[_ cs] (cs/read cs (first str))]
        (recur (next str) cs))))))

(defn skip-string
  "Skips the `string`"
  [string]
  (fn [cs]
    (loop [str string
           cs  cs]
      (if (empty? str)
        (jb/ok nil cs)
      (when-let [[_ cs] (cs/read cs (first str))]
        (recur (next str) cs))))))
