(ns jdsl.combinator
  (:refer-clojure :exclude [apply map peek])
  (:require [jdsl.basic :as jb :only [run return]]))

(defn <$>
 "Applies a function `f` to the result of parsing with parser `p`.
  Returns a new parser that applies `p` to the input string `ts`,
  then applies `f` to the first result and returns a list of the
  transformed result and the remaining input string.

  Examples:  
  ```clojure
  (def parser (char \\a))  
  (def mapped-parser (<$> str parser))  
  (jb/run mapped-parser \"abc\") ;=> (\"a\" \"bc\")  
  ```
  "
  [f p]
  (fn [ts]
    (let [[a ts] (jb/run p ts)]
    (jb/return (f a) ts))))

(defn <$
  "(<$) :: a -> p b -> p a"
  [a p]
  (fn [ts]
    (let [[_ ts] (jb/run p ts)]
    (jb/return a ts))))

(defn $>
  "($>) :: p a ->  b -> p b"
  [p b]
  (fn [ts]
    (let [[_ ts] (jb/run p ts)]
    (jb/return b ts))))

(defn return
  "Wraps a value `a` into parser `p`
   return :: a -> p a"
  [a]
  (fn [ts]
    (jb/return a ts)))

(defn zero
  "parser that always returns ParserError = nil"
  [_] nil)

(defn fail
  "parser that always returns ParserError = str"
  [msg]
  (fn [_] msg))

(defn lift
  "lift :: (a -> b) -> (a -> p b)"
  [f]
  (fn [a]
    (return (f a))))

(defn >>=
  "(>>=) :: p a -> (a -> p b) -> p b"
  [p f]
  (fn [ts]
    (let [[a ts] (jb/run p ts)
          [b ts] (jb/run (f a) ts)]
    (jb/return b ts))))

(defn =<<
  "(=<<) :: (a -> p b) -> p a -> p b"
  [f p]
  (fn [ts]
    (let [[a ts] (jb/run p ts)
          [b ts] (jb/run (f a) ts)]
    (jb/return b ts))))

(defn >>
  "(>>) :: p a -> p b -> p b"
  [pa pb]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)
          [b ts] (jb/run pb ts)]
    (jb/return b ts))))

(defn <<
  "(<<) :: p a -> p b -> p a"
  [pa pb]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)
          [_ ts] (jb/run pb ts)]
    (jb/return a ts))))

(defn <*>
  "(<*>) :: Monoid p => p (a -> b) -> p a -> p b"
  [pf pa]
  (fn [ts]
    (let [[f ts] (jb/run pf ts)
          [a ts] (jb/run pa ts)]
    (jb/return (f a) ts))))

(defn <>
  "(<>) :: Monoid p => p a -> p b -> p (a b)
   (<>) :: Monoid p => f -> p a -> p b -> p (f a b)"
  ([pa pb]
    (<> list pa pb))
  ([f pa pb]
    (fn [ts]
      (let [[a ts] (jb/run pa ts)
            [b ts] (jb/run pb ts)]
      (jb/return (f a b) ts)))))

(defn <?>
 "Returns a parser that takes an input `ts` and runs the parser `p` on it.
  If the parser succeeds, returns the parsed result. Otherwise, throws an  
  error with the message `msg`.

  Examples:  
  ```clojure
  (def parser (string \"hello\"))
  ((<?> parser \"Parse failed\") \"hello\") ; => (\"hello\", nil)
  ((<?> parser \"Parse failed\") \"world\") ; => Error: Parse failed
  ```
  "
  [p msg]
  (fn [ts]
    (try
      (jb/run p ts)
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
      (throw (jb/parse-error (str msg \newline " " (:msg data))
                             (:ts data))))))))

(defn <|>
 "Tries to apply the parser `pa` to the input `ts`. If `pa` succeeds, returns a list with the result of `pa` and the remaining input `ts`. Otherwise, applies the parser `pb` to the input `ts` and returns its result.

  Examples:  
  ```clojure
  (def p1 (char \\a))
  (def p2 (char \\b))
  (jb/run (<|> p1 p2) \"abc\") ; => (\\a \"bc\")
  (jb/run (<|> p1 p2) \"bcd\") ; => (\\b \"cd\")
  ```
  "
  [pa pb]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if-not (nil? a)
      (jb/return a ts)
    (jb/run pb ts)))))

(defn optional
 "The parser `optional` takes a parser `p` as input and returns a new parser  
  that skips over an optional occurrence of `p`.  If `p` is present,  
  it is consumed and the result is returned. If `p` is not present, `nil` is returned.

  Examples:  
  ```clojure
  (def a (char-parser \\a))  
  (def b (char-parser \\b))  

  (jb/run (optional a) \"abc\") ; returns (\"a\" \"bc\")  
  (jb/run (optional a) \"bc\") ; returns (nil \"bc\")  
  (jb/run (optional b) \"abc\") ; returns (nil \"abc\")  
  (jb/run (optional b) \"bc\") ; returns (\"a\" \"bc\")  
  ```
  "
  [p]
  (fn [ts]
    (let [[_ ts] (jb/attempt p ts)]
    (jb/return nil ts))))

(defn peek
  "Returns a parser that matches the given parser `p`, but does not consume any input.  
   
   Example:  
   (def p (peek (char \\a)))  
   (jb/run p \"abc\") ; => (\\a, \"abc\")  
   (jb/run p \"efg\") ; => (nil, \"efg\")
   "
  [p]
  (fn [ts]
    (let [[a _] (jb/attempt p ts)]
    (jb/return a ts))))

(defn between
 "The `between` function takes in three parsers `pb`, `pa`, and `pc` and applies them in sequence.  
  It returns the result of `pb`. The purpose of this function is to parse a sequence of tokens that  
  are enclosed between two specific tokens.  

  Args:  
  - `pa`: The parser for the opening token.  
  - `pb`: The parser for the tokens between the opening and closing tokens.  
  - `pc`: The parser for the closing token.  

  Returns:  
  - The result of `pb`.  

  Examples:  
  ```clojure
  (def digit (one-of \"0123456789\"))
  (def digits (many (digit)))
  (def parser (between (char \\() digits (char \\))))

  (jb/run parser \"(123)\") ; Returns: (\"123\", nil)
  (jb/run parser \"(456)\") ; Returns: (\"456\", nil)
  ```
  "
  [pb pa pc]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)
          [b ts] (jb/run pb ts)
          [_ ts] (jb/run pc ts)]
    (jb/return b ts))))

(defn many*
  "many applies the parser `p` zero or more times"
  [p]
  (fn [ts]
    (loop [ts ts
           as []]
      (let [[a ts] (jb/attempt p ts)]
      (if (nil? a)
        (jb/return as ts)
      (recur ts (conj as a)))))))

(defn many+
  "many applies the parser `p` one or more times"
  [p]
  (fn [ts]
    (let [[a  ts] (jb/run p ts)
          [bs ts] (jb/run (many* p) ts)]
    (jb/return (cons a bs) ts))))

(defn choice
  "The parser choice ps is an optimized implementation of p1 <|> p2 <|> ... <|> pn , where p1 … pn are the parsers in the sequence ps."
  [ps]
  (if (empty? ps)
    zero
  (fn [ts]
    (loop [ps ps
           a  nil
           ts ts]
      (if (empty? ps)
        (or a (jb/return a ts)) ;; ParseError = nil if a is nil after running all the `ps`
      (let [[p & ps] ps
            [a ts]   (jb/attempt p ts)]
      (if-not (nil? a)
        (jb/return a ts)
      (recur ps a ts))))))))

(defn followed-by
  "The parser followedBy p succeeds if the parser p succeeds at the current position.  
   Otherwise it fails with error. This parser never changes the parser state."
  [p]
  (fn [ts]
    (when (jb/run p ts)
      (jb/return nil ts))))

(defn not-followed-by
  "The parser notFollowedBy p succeeds if the parser p fails to parse at the current position.  
   Otherwise it fails with an error. This parser never changes the parser state."
  [p]
  (fn [ts]
    (let [[a _] (jb/attempt p ts)]
    (when (nil? a) 
      (jb/return nil ts)))))

(defn skip-many*
  "The parser `(skip-many* p)` is an optimized implementation of  `(fn [_]) <$> many* p`"
  [p]
  (fn [ts]
    (loop [ts ts]
      (let [[a ts] (jb/attempt p ts)]
      (if (nil? a)
        (jb/return nil ts)
      (recur ts))))))

(defn skip-many+*
  "The parser `(skip-many+ p)` is an optimized implementation of `(fn [_]) <$> many+ p`"
  [p]
  (fn [ts]
    (let [[_ ts] (jb/run p ts)
          [_ ts] (jb/run (skip-many* p) ts)]
    (jb/return nil ts))))

(defn sep-by*
  "sepBy p sep parses zero or more occurrences of p, separated by sep."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/return nil ts)
    (loop [ts ts
           as [a]]
      (let [[a ts] (jb/attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (jb/return as ts)
      (recur ts (conj as a)))))))))

(defn sep-by+
  "The parser sepBy1 p sep parses one or more occurrences of p separated by sep."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)]
    (loop [ts ts
           as [a]]
      (let [[a ts] (jb/attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (jb/return as ts)
      (recur ts (conj as a))))))))

(defn skip-sep-by*
  "The parser skip-sep-by* p sep is an optimized implementation of (fn [_]) <$> sep-by* p sep."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/return nil ts)
    (loop [ts ts]
      (let [[a ts] (jb/attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (jb/return nil ts)
      (recur ts))))))))

(defn skip-sep-by+
  "The parser skipSepBy+ p sep is an optimized implementation of (fn [_]) <$> sepBy+ p sep."
  [pa ps]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)]
      (loop [ts ts]
        (let [[a ts] (jb/attempt (-> ps (>> pa)) ts)]
        (if (nil? a)
          (jb/return nil ts)
        (recur ts)))))))

(defn sep-end-by*
  "The parser sepEndBy p sep parses zero or more occurrences of p separated and  
   optionally ended by sep. It returns a list of the results returned by p."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/return nil ts)
    (loop [ts ts
           as [a]]
      (let [[a ts] (jb/attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (let [[_ ts] (jb/attempt ps ts)] ;; optionally ended by sep
        (jb/return as ts))
      (recur ts (conj as a)))))))))

(defn sep-end-by+
  "The parser sepEndBy1 p sep parses one or more occurrences of p separated and  
   optionally ended by sep.  It returns a list of the results returned by p."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)]
    (loop [ts ts
           as [a]]
      (let [[a ts] (jb/attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (let [[_ ts] (jb/attempt ps ts)] ;; optionally ended by sep
        (jb/return as ts))
      (recur ts (conj as a))))))))

(defn skip-sep-end-by*
  "The parser skipSepEndBy p sep is an optimized implementation of sepEndBy p sep |>> ignore."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/return nil ts)
    (loop [ts ts]
      (let [[a ts] (jb/attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (let [[_ ts] (jb/attempt ps ts)] ;; optionally ended by sep
        (jb/return nil ts))
      (recur ts))))))))

(defn skip-sep-end-by+
  "The parser skipSepEndBy1 p sep is an optimized implementation of sepEndBy1 p sep |>> ignore."
  [pa ps]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)]
    (loop [ts ts]
      (let [[a ts] (jb/attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (let [[_ ts] (jb/attempt ps ts)] ;; optionally ended by sep
        (jb/return nil ts))
      (recur ts)))))))

(defn many-till*
  "manyTill p end applies parser p zero or more times until parser end succeeds.  
   Returns the list of values returned by p."
  [pa pe]
  (fn [ts]
    (loop [ts ts
           as []]
      (let [[e ts] (jb/attempt pe ts)]
      (if-not (nil? e)
        (jb/return (not-empty as) ts)
      (let [[a ts] (jb/run pa ts)]
      (recur ts (conj as a))))))))

(defn many-till+
  "manyTill1 p end applies parser p one or more times until parser end succeeds.  
   Returns the list of values returned by p."
  [pa pe]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)]
    (loop [ts ts
           as [a]]
      (let [[e ts] (jb/attempt pe ts)]
      (if-not (nil? e)
        (jb/return as ts)
      (let [[a ts] (jb/run pa ts)]
      (recur ts (conj as a)))))))))

(defn skip-many-till*
  "skipManyTill p end applies parser p zero or more times until parser end succeeds.  
   Returns the list of values returned by p."
  [pa pe]
  (fn [ts]
    (loop [ts ts]
      (let [[e ts] (jb/attempt pe ts)]
      (if-not (nil? e)
        (jb/return nil ts)
      (let [[_ ts] (jb/run pa ts)]
      (recur ts)))))))

(defn skip-many-till+
  "skipManyTill1 p end applies parser p one or more times until parser end succeeds.  
   Returns the list of values returned by p."
  [pa pe]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)]
    (loop [ts ts]
      (let [[e ts] (jb/attempt pe ts)]
      (if-not (nil? e)
        (jb/return nil ts)
      (let [[_ ts] (jb/run pa ts)]
      (recur ts))))))))

(defmacro copy-meta
  "Copy meta-data from `from-var` to `to-var`"
  ^:private
  [sym value]
  `(alter-meta! ~sym merge (meta (var ~value))))

(-> (def bind  >>=) (copy-meta >>=))
(-> (def alt   <|>) (copy-meta <|>))
(-> (def label <?>) (copy-meta <?>))
(-> (def map   <$>) (copy-meta <$>))
(-> (def apply <*>) (copy-meta <*>))
