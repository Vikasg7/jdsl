(ns jdsl.parser
  (:refer-clojure :exclude [char])
  (:require [jdsl.token-stream :as ts]
            [jdsl.basic :as jb :refer [return error EOS?]]))

(defn char
  "Parses a character `c`, (\\r\\n, \\r, \\n) to \\newline. 
   Examples:  
   ```clojure
   (run (char \\a) (ts/create \"abc\")) ; => [\\a [\"bc\" 0]]
   (run (char \\newline) (ts/create \"\r\nc\")) ; => [\\newline [\"c\" 1]]
   (run (char \\c) (ts/create \"abc\")) ; => \"Expected: c\\nFound: a\"
   (run (char \\c) (ts/create nil)) ; => \"Expected: c\\nFound: End of Stream\"
   ```"
  [c]
  (fn [ts]
    (if (or (= c \return) (= c \newline))
      (if-let [ts (ts/skip-newline ts)]
        (return \newline ts)
      (return (error "Expected: " c)))
    (let [token (ts/peek ts)]
    (if (EOS? token)
      (return (error "Expected: " c))
    (if (= c token)
      (return c (ts/skip ts 1))
    (return (error "Expected: " c))))))))

(defn skip-char
  "Skips the character `c`"
  [c]
  (fn [ts]
    (if (or (= c \return) (= c \newline))
      (if-let [ts (ts/skip-newline ts)]
        (return nil ts)
      (return (error "Expected: " c)))
    (let [token (ts/peek ts)]
    (if (EOS? token)
      (return (error "Expected: " c))
    (if (= c token)
      (return nil (ts/skip ts 1))
    (return (error "Expected: " c))))))))

(defn any-char
  "Parses any character, taking care of newline characters"
  [ts]
  (let [[token ts] (ts/read ts)]
  (if (EOS? token)
    (return (error "Expected: Any Character"))
  (return token ts))))

