(ns jdsl.core
  (:refer-clojure :exclude [do map peek apply]))


;; Success a ts :: (list a ts)
;; Error        :: nil | string
;; Parser a ts  :: ts -> Success a ts | Error

(def ^:private error? string?)

(def ^:private error identity)

;; TODO: redo print-error based on the new token-stream.clj
;; Check if the input is str else don't calcuate the line-idx or col-idx
(defn print-error
  "Prints the message based on original input `ts`, remaining input `ts` and error `msg`."
  [e]
  (let [ts      (:ts (ex-data e))
        msg     (ex-message e)
        err-idx (- (count ts) (count ts))]
  (println msg " while parsing: " (nth ts 0) " at index: " err-idx)))

(defn parse-error
  "Helper function to generate parsing error"
  [msg ts]
  (ex-info "ParseError" {:ts ts :msg msg}))

(defn run
  "Runs the parser `p` on input `ts`, throws parsing error"
  ([p]
    (fn [ts]
      (run p ts)))
  ([p ts]
    (let [result (p ts)]
    (if (or (nil? result) (error? result))
      (-> result)
    (throw (parse-error result ts))))))

(declare many+)

(defn run-all
  "Runs the parse with the given input until the input is consumed."
  [p ts]
  (run (many+ p) ts))

(defmacro ignore-parse-error 
  "macro to catch-ignore only parse errors and throw rest of them."
  [& body]
  `(try 
     ~@body
   (catch clojure.lang.ExceptionInfo ~'e
     (when (not= "ParseError" (ex-message ~'e))
       (throw ~'e)))))

(defn attempt
  "Attempt to run the `p` on input `ts`, backtracks if fails."
  ([p]
    (fn [ts] (attempt p ts)))
  ([p ts]
    (if-let [result (ignore-parse-error (run p ts))]
      (-> result)
    (list nil ts))))

(defn <$>
 "Applies a function `f` to the result of parsing with parser `p`.
  Returns a new parser that applies `p` to the input string `ts`,
  then applies `f` to the first result and returns a list of the
  transformed result and the remaining input string.

  Examples:  
  ```clojure
  (def parser (char \\a))  
  (def mapped-parser (<$> str parser))  
  (run mapped-parser \"abc\") ;=> (\"a\" \"bc\")  
  ```
  "
  [f p]
  (fn [ts]
    (let [[a ts] (run p ts)]
    (list (f a) ts))))

(defn <$
  "(<$) :: a -> p b -> p a"
  [a p]
  (fn [ts]
    (let [[_ ts] (run p ts)]
    (list a ts))))

(defn $>
  "($>) :: p a ->  b -> p b"
  [p b]
  (fn [ts]
    (let [[_ ts] (run p ts)]
    (list b ts))))

(defn return
  "Wraps a value `a` into parser `p`
   return :: a -> p a"
  [a]
  (fn [ts]
    (list a ts)))

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
    (let [[a ts] (run p ts)
          [b ts] (run (f a) ts)]
    (list b ts))))

(defn =<<
  "(=<<) :: (a -> p b) -> p a -> p b"
  [f p]
  (fn [ts]
    (let [[a ts] (run p ts)
          [b ts] (run (f a) ts)]
    (list b ts))))

(defn >>
  "(>>) :: p a -> p b -> p b"
  [pa pb]
  (fn [ts]
    (let [[_ ts] (run pa ts)
          [b ts] (run pb ts)]
    (list b ts))))

(defn <<
  "(<<) :: p a -> p b -> p a"
  [pa pb]
  (fn [ts]
    (let [[a ts] (run pa ts)
          [_ ts] (run pb ts)]
    (list a ts))))

(defn <*>
  "(<*>) :: Monoid p => p (a -> b) -> p a -> p b"
  [pf pa]
  (fn [ts]
    (let [[f ts] (run pf ts)
          [a ts] (run pa ts)]
    (list (f a) ts))))

(defn <>
  "(<>) :: Monoid p => p a -> p b -> p (a b)
   (<>) :: Monoid p => f -> p a -> p b -> p (f a b)"
  ([pa pb]
    (<> list pa pb))
  ([f pa pb]
    (fn [ts]
      (let [[a ts] (run pa ts)
            [b ts] (run pb ts)]
      (list (f a b) ts)))))

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
    (if-let [result (run p ts)]
      (-> result)
    (error msg))))

(defn <|>
 "Tries to apply the parser `pa` to the input `ts`. If `pa` succeeds, returns a list with the result of `pa` and the remaining input `ts`. Otherwise, applies the parser `pb` to the input `ts` and returns its result.

  Examples:  
  ```clojure
  (def p1 (char \\a))
  (def p2 (char \\b))
  (run (<|> p1 p2) \"abc\") ; => (\\a \"bc\")
  (run (<|> p1 p2) \"bcd\") ; => (\\b \"cd\")
  ```
  "
  [pa pb]
  (fn [ts]
    (let [[a ts] (attempt pa ts)]
    (if-not (nil? a)
      (list a ts)
    (run pb ts)))))

(defn optional
 "The parser `optional` takes a parser `p` as input and returns a new parser  
  that skips over an optional occurrence of `p`.  If `p` is present,  
  it is consumed and the result is returned. If `p` is not present, `nil` is returned.

  Examples:  
  ```clojure
  (def a (char-parser \\a))  
  (def b (char-parser \\b))  

  (run (optional a) \"abc\") ; returns (\"a\" \"bc\")  
  (run (optional a) \"bc\") ; returns (nil \"bc\")  
  (run (optional b) \"abc\") ; returns (nil \"abc\")  
  (run (optional b) \"bc\") ; returns (\"a\" \"bc\")  
  ```
  "
  [p]
  (fn [ts]
    (let [[_ ts] (attempt p ts)]
    (list nil ts))))

(defn peek
  "Returns a parser that matches the given parser `p`, but does not consume any input.  
   
   Example:  
   (def p (peek (char \\a)))  
   (run p \"abc\") ; => (\\a, \"abc\")  
   (run p \"efg\") ; => (nil, \"efg\")
   "
  [p]
  (fn [ts]
    (let [[a _] (attempt p ts)]
    (list a ts))))

(defn between
 "The `between` function takes in three parsers `pa`, `pb`, and `pc` and applies them in sequence.  
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

  (run parser \"(123)\") ; Returns: (\"123\", nil)
  (run parser \"(456)\") ; Returns: (\"456\", nil)
  ```
  "
  [pa pb pc]
  (fn [ts]
    (let [[_ ts] (run pa ts)
          [b ts] (run pb ts)
          [_ ts] (run pc ts)]
    (list b ts))))

(defn many*
  "many applies the parser `p` zero or more times"
  [p]
  (fn [ts]
    (loop [ts ts
           as []]
      (let [[a ts] (attempt p ts)]
      (if (nil? a)
        (list as ts)
      (recur ts (conj as a)))))))

(defn many+
  "many applies the parser `p` one or more times"
  [p]
  (fn [ts]
    (let [[a  ts] (run p ts)
          [bs ts] (run (many* p) ts)]
    (list (cons a bs) ts))))

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
        (or a (list a ts)) ;; ParseError = nil if a is nil after running all the `ps`
      (let [[p & ps] ps
            [a ts]   (attempt p ts)]
      (if-not (nil? a)
        (list a ts)
      (recur ps a ts))))))))

(defn followed-by
  "The parser followedBy p succeeds if the parser p succeeds at the current position.  
   Otherwise it fails with error. This parser never changes the parser state."
  [p]
  (fn [ts]
    (when (run p ts)
      (list nil ts))))

(defn not-followed-by
  "The parser notFollowedBy p succeeds if the parser p fails to parse at the current position.  
   Otherwise it fails with an error. This parser never changes the parser state."
  [p]
  (fn [ts]
    (let [[a _] (attempt p ts)]
    (when (nil? a) 
      (list nil ts)))))

(defn skip-many*
  "The parser `(skip-many* p)` is an optimized implementation of  `(fn [_]) <$> many* p`"
  [p]
  (fn [ts]
    (loop [ts ts]
      (let [[a ts] (attempt p ts)]
      (if (nil? a)
        (list nil ts)
      (recur ts))))))

(defn skip-many+*
  "The parser `(skip-many+ p)` is an optimized implementation of `(fn [_]) <$> many+ p`"
  [p]
  (fn [ts]
    (let [[_ ts] (run p ts)
          [_ ts] (run (skip-many* p) ts)]
    (list nil ts))))

(defn sep-by*
  "sepBy p sep parses zero or more occurrences of p, separated by sep."
  [pa ps]
  (fn [ts]
    (let [[a ts] (attempt pa ts)]
    (if (nil? a)
      (list nil ts)
    (loop [ts ts
           as [a]]
      (let [[a ts] (attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (list as ts)
      (recur ts (conj as a)))))))))

(defn sep-by+
  "The parser sepBy1 p sep parses one or more occurrences of p separated by sep."
  [pa ps]
  (fn [ts]
    (let [[a ts] (run pa ts)]
    (loop [ts ts
           as [a]]
      (let [[a ts] (attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (list as ts)
      (recur ts (conj as a))))))))

(defn skip-sep-by*
  "The parser skip-sep-by* p sep is an optimized implementation of (fn [_]) <$> sep-by* p sep."
  [pa ps]
  (fn [ts]
    (let [[a ts] (attempt pa ts)]
    (if (nil? a)
      (list nil ts)
    (loop [ts ts]
      (let [[a ts] (attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (list nil ts)
      (recur ts))))))))

(defn skip-sep-by+
  "The parser skipSepBy+ p sep is an optimized implementation of (fn [_]) <$> sepBy+ p sep."
  [pa ps]
  (fn [ts]
    (let [[_ ts] (run pa ts)]
      (loop [ts ts]
        (let [[a ts] (attempt (-> ps (>> pa)) ts)]
        (if (nil? a)
          (list nil ts)
        (recur ts)))))))

(defn sep-end-by*
  "The parser sepEndBy p sep parses zero or more occurrences of p separated and  
   optionally ended by sep. It returns a list of the results returned by p."
  [pa ps]
  (fn [ts]
    (let [[a ts] (attempt pa ts)]
    (if (nil? a)
      (list nil ts)
    (loop [ts ts
           as [a]]
      (let [[a ts] (attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (let [[_ ts] (attempt ps ts)] ;; optionally ended by sep
        (list as ts))
      (recur ts (conj as a)))))))))

(defn sep-end-by+
  "TThe parser sepEndBy1 p sep parses one or more occurrences of p separated and  
   optionally ended by sep.  It returns a list of the results returned by p."
  [pa ps]
  (fn [ts]
    (let [[a ts] (run pa ts)]
    (loop [ts ts
           as [a]]
      (let [[a ts] (attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (let [[_ ts] (attempt ps ts)] ;; optionally ended by sep
        (list as ts))
      (recur ts (conj as a))))))))

(defn skip-sep-end-by*
  "The parser skipSepEndBy p sep is an optimized implementation of sepEndBy p sep |>> ignore."
  [pa ps]
  (fn [ts]
    (let [[a ts] (attempt pa ts)]
    (if (nil? a)
      (list nil ts)
    (loop [ts ts]
      (let [[a ts] (attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (let [[_ ts] (attempt ps ts)] ;; optionally ended by sep
        (list nil ts))
      (recur ts))))))))

(defn skip-sep-end-by+
  "The parser skipSepEndBy1 p sep is an optimized implementation of sepEndBy1 p sep |>> ignore."
  [pa ps]
  (fn [ts]
    (let [[_ ts] (run pa ts)]
    (loop [ts ts]
      (let [[a ts] (attempt (-> ps (>> pa)) ts)]
      (if (nil? a)
        (let [[_ ts] (attempt ps ts)] ;; optionally ended by sep
        (list nil ts))
      (recur ts)))))))

(defn many-till*
  "manyTill p end applies parser p zero or more times until parser end succeeds.  
   Returns the list of values returned by p."
  [pa pe]
  (fn [ts]
    (loop [ts ts
           as []]
    (let [[e ts] (attempt pe ts)]
    (if-not (nil? e)
      (list (not-empty as) ts)
    (let [[a ts] (run pa ts)]
    (recur ts (conj as a))))))))

(defn many-till+
  "manyTill1 p end applies parser p one or more times until parser end succeeds.  
   Returns the list of values returned by p."
  [pa pe]
  (fn [ts]
    (let [[a ts] (run pa ts)]
    (loop [ts ts
           as [a]]
    (let [[e ts] (attempt pe ts)]
    (if-not (nil? e)
      (list as ts)
    (let [[a ts] (run pa ts)]
    (recur ts (conj as a)))))))))

(defn skip-many-till*
  "skipManyTill p end applies parser p zero or more times until parser end succeeds.  
   Returns the list of values returned by p."
  [pa pe]
  (fn [ts]
    (loop [ts ts]
    (let [[e ts] (attempt pe ts)]
    (if-not (nil? e)
      (list nil ts)
    (let [[_ ts] (run pa ts)]
    (recur ts)))))))

(defn skip-many-till+
  "skipManyTill1 p end applies parser p one or more times until parser end succeeds.  
   Returns the list of values returned by p."
  [pa pe]
  (fn [ts]
    (let [[_ ts] (run pa ts)]
    (loop [ts ts]
    (let [[e ts] (attempt pe ts)]
    (if-not (nil? e)
      (list nil ts)
    (let [[_ ts] (run pa ts)]
    (recur ts))))))))

(def bind  >>=)
(def alt   <|>)
(def label <?>)
(def map   <$>)
(def apply <*>)

(defn- expand 
  "Expands (a <- parser) or (parser) bindings in the do macro"
  [[sym op prsr :as expr]]
  {:pre [(> (count expr) 0)]}
  (if (= op '<-)
    [[sym 'ts] (list run prsr 'ts)]
  [['_ 'ts] (list run expr 'ts)]))

(defmacro do- 
  "Haskel like do macro to abstract away passing around `ts` and calling `run` function."
  [& exprs]
  (let [bindings (mapcat expand exprs)]
  `(fn [~'ts]
    (let [~@bindings]
    (list ~'_ ~'ts)))))
