(ns jdsl.parser
  (:refer-clojure :exclude [char])
  (:require [jdsl.token-stream :as ts]
            [jdsl.basic :as jb]
            [jdsl.basic :refer [return error EOS?]]
            [clojure.string :as str]))

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
      (return (error "Expected: " c "\nFound: " (ts/peek ts))))
    (let [token (ts/peek ts)]
    (if (EOS? token)
      (return (error "Expected: " c "\nFound: End of Stream"))
    (if (= c token)
      (return c (ts/skip ts 1))
    (return (error "Expected: " c "\nFound: " token))))))))

