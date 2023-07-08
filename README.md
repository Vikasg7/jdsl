# JDSL
A Parser Combinator Library in Clojure  

### Definitions  
A _Parser_ is a function that takes a char stream as an argument, for example string, and returns the parsed character/characters and rest of the char stream. From Clojure's POV, it looks something like this:  
```clojure
(def parser
  (fn [char-stream]
    ;; Do something with the char-stream to parse a value
    (vector value char-stream))) ;; updated char-stream = [string position]
```
A _Parser Combinator_, on the other hand, is a higher order function (a function that takes other functions as an argument, for example, `map`, `filter`) that takes A Parser (A function) as an argument and returns another Parser. In that sense, A Parser Combinator is something that operates on Parsers. A few examples of Parser Combinators on this library can be - `choice`, `between`, `optional`, `many*`, `many+` etc. Lets see an example.  
```clojure
(ns examples.readme-example
  (:require [jdsl.char-parser :as jp]
            [jdsl.char-stream :as cs]
            [jdsl.basic       :as jb]
            [jdsl.combinator :refer [between many*]]))

(def any-char-within-braces
  (-> (many* jp/alpha-num)
      (between (jp/char \[) (jp/char \]))))

(try
  (jb/run any-char-within-braces (cs/create "[abcd123]"))
(catch clojure.lang.ExceptionInfo e
  (jb/print-error e)))
;; returns [\a \b \c \d \1 \2 \3] as parsed value
;;         [\[ \a \b \c \d \1 \2 \3 \] 8] as final char stream state
```

### Ingredients (Available Namespaces/Modules)  
TODO

### Sources
- [FParsec Documentation](http://www.quanttec.com/fparsec/reference/primitives.html)  
- [Parsec Documentation](https://hackage.haskell.org/package/parsec-3.1.16.1/docs/)

### TODO
- [ ] Add custom .clj-kondo hook for jdsl.basic/do macro