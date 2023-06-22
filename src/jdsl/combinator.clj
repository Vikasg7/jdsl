(ns jdsl.combinator
  (:refer-clojure :exclude [apply map peek])
  (:require [jdsl.basic :as jb]
            [clojure.string :as str]))

(defn <$>
  "`(<$>) :: (a -> b) -> p a -> p b`  
   Applies the function `f` on the result `a` of running the parser `p`
   to produce the parser `p b`.  
   `<Flipable>`"
  [f p]
  (fn [ts]
    (let [[a ts] (jb/run p ts)]
    (jb/ok (f a) ts))))

(defn <$
  "`(<$) :: a -> p b -> p a`  
   Runs parser `p a`, if successful, replaces the results `b` with `a`.  
   `<Flipable>`"
  [a p]
  (fn [ts]
    (let [[_ ts] (jb/run p ts)]
    (jb/ok a ts))))

(defn $>
  "`($>) :: p a -> b -> p b`  
   Runs parser `p a`, if successful, replaces the result `a` with `b`.  
   `<Flipable>`"
  [p b]
  (fn [ts]
    (let [[_ ts] (jb/run p ts)]
    (jb/ok b ts))))

(defn return
  "`return :: a -> p a`  
   Wraps a value `a` into parser `p`"
  [a]
  (fn [ts]
    (jb/ok a ts)))

(def zero
  "`zero` parser fails with ParseError = nil."
  (fn [_] (jb/error nil)))

(defn fail-with
  "`fail-with` parser fails with ParseError = `msg`."
  [msg]
  (fn [_] (jb/error msg)))

(defn lift
  "`lift :: (a -> b) -> (a -> p b)`  
   Lifts a function `f` to a function that, when passed `a`,
   returns a parser `p b`"
  [f]
  (fn [a]
    (return (f a))))

(defn >>=
  "`(>>=) :: p a -> (a -> p b) -> p b`  
   Monadic Bind Operation. Runs the parser `p a`, passes the result `a` to `f`
   to produce parser `p b`, runs it and returns the result `b`.  
   `<Flipable>`"
  [p f]
  (fn [ts]
    (let [[a ts] (jb/run p ts)
          [b ts] (jb/run (f a) ts)]
    (jb/ok b ts))))

(defn =<<
  "`(=<<) :: (a -> p b) -> p a -> p b`  
   Runs the parser `p a`, passes the result `a` to `f` to produce parser `p b`,
   runs it and returns the result `b`  
   `<Flipable>`"
  [f p]
  (fn [ts]
    (let [[a ts] (jb/run p ts)
          [b ts] (jb/run (f a) ts)]
    (jb/ok b ts))))

(defn >>
  "`(>>) :: p a -> p b -> p b`  
   Runs parsers `pa` and `pb` in sequence and returns the result `b`.  
   `<Flipable>`"
  [pa pb]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)
          [b ts] (jb/run pb ts)]
    (jb/ok b ts))))

(defn <<
  "`(<<) :: p a -> p b -> p a`  
   Runs parsers `pa` and `pb` in sequence and returns the result `a`.  
   `<Flipable>`"
  [pa pb]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)
          [_ ts] (jb/run pb ts)]
    (jb/ok a ts))))

(defn <*>
  "`(<*>) :: Monoid p => p (a -> b) -> p a -> p b`  
   Runs parser `pf` to get `f` and parser `pa` to get a and returns
   the result of calling `f` with `a`.  
   `<Flipable>`"
  [pf pa]
  (fn [ts]
    (let [[f ts] (jb/run pf ts)
          [a ts] (jb/run pa ts)]
    (jb/ok (f a) ts))))

(defn <>
  "`(<>) :: Monoid p => p a -> p b -> p (a b)`  
   Runs parser `pa` to get `a` and parser `pb` to get `b` and returns the
   result `(list a b)`.

   `(<>) :: Monoid p => f -> p a -> p b -> p (f a b)`  
   Runs parser `pa` to get `a` and parser `pb` to get `b` and returns the
   result calling `f` with `a` and `b` as arguments."
  ([pa pb]
    (<> list pa pb))
  ([f pa pb]
    (fn [ts]
      (let [[a ts] (jb/run pa ts)
            [b ts] (jb/run pb ts)]
      (jb/ok (f a b) ts)))))

(defn <?>
  "`(<?>) :: p a -> string -> p a`  
   Returns parser `p` that fails with the `msg`.  
   `<Flipable>`"
  [p msg]
  (fn [ts]
    (try
      (jb/run p ts)
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)
            msgs  (if (:msg data) [msg (:msg data)] [msg])]
      (throw (jb/parse-error (str/join "\n " msgs)
                             (:ts data))))))))

(defn <|>
  "`(<|>) :: p a -> p b -> p (a|b)`  
   Run parser `p a`, if successful, returns `a`, otherwise runs parser `p b` and retuns `b`"
  [pa pb]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if-not (nil? a)
      (jb/ok a ts)
    (jb/run pb ts)))))

(defn optional
  "Checks for an optional occurrence of `p a` and skips it."
  [p]
  (fn [ts]
    (let [[_ ts] (jb/attempt p ts)]
    (jb/ok nil ts))))

(defn attempt
  "Tries the parser `p` and backtracks if fails."
  [p]
  (fn [ts]
    (if-let [result (jb/ignore-parse-error (jb/run p ts))]
      (-> result)
    (vector nil ts))))

(defn peek
  "Returns the result of running the parser `p`, doesn't consume input.
   Returns nil if failed."
  [p]
  (fn [ts]
    (let [[a _] (jb/run (attempt p) ts)]
    (jb/ok a ts))))

(defn between
  "Runs parsers `pa`, `pb` and `pc` in sequence and returns the result of `pb`.  
   `<Flipable>`"
  [pb pa pc]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)
          [b ts] (jb/run pb ts)
          [_ ts] (jb/run pc ts)]
    (jb/ok b ts))))

(defn many*
  "Applies the parser `p` zero or more times, collect and returns the results in vector."
  [p]
  (fn [ts]
    (loop [ts ts
           as []]
      (let [[a ts] (jb/attempt p ts)]
      (if (nil? a)
        (jb/ok (not-empty as) ts)
      (recur ts (conj as a)))))))

(defn many+
  "Applies the parser `p` one or more times, collect and returns the results in vector."
  [p]
  (fn [ts]
    (let [[a ts] (jb/run p ts)]
    (loop [ts ts
           as [a]]
      (let [[a ts] (jb/attempt p ts)]
      (if (nil? a)
        (jb/ok as ts)
      (recur ts (conj as a))))))))

(defn choice
  "Runs a list of parsers in a sequence and returns the result of first successful parser."
  [ps]
  (if (empty? ps)
    zero
  (fn [ts]
    (loop [ps ps
           ts ts]
      (when-not (empty? ps)
        (let [[p & ps] ps
              [a ts]   (jb/attempt p ts)]
        (if-not (nil? a)
          (jb/ok a ts)
        (recur ps ts))))))))

(defn followed-by
  "The parser `(followed-by p)` succeeds if the parser `p` succeeds at the current position.
   Otherwise it fails with error. This parser never changes the parser state."
  [p]
  (fn [ts]
    (when (jb/run p ts)
      (jb/ok nil ts))))

(defn not-followed-by
  "The parser `(not-followed-by p)` succeeds if the parser `p` fails to parse at the current position.
   Otherwise it fails with an error. This parser never changes the parser state."
  [p]
  (fn [ts]
    (let [[a _] (jb/attempt p ts)]
    (when (nil? a) 
      (jb/ok nil ts)))))

(defn skip-many*
  "The parser `(skip-many* p)` is an optimized implementation of  `(-> (fn [_]) (<$> many* p))`"
  [p]
  (fn [ts]
    (loop [ts ts]
      (let [[a ts] (jb/attempt p ts)]
      (if (nil? a)
        (jb/ok nil ts)
      (recur ts))))))

(defn skip-many+
  "The parser `(skip-many+ p)` is an optimized implementation of `(-> (fn [_]) (<$> many+ p))`"
  [p]
  (fn [ts]
    (let [[_ ts] (jb/run p ts)]
    (loop [ts ts]
      (let [[a ts] (jb/attempt p ts)]
      (if (nil? a)
        (jb/ok nil ts)
      (recur ts)))))))

(defn sep-by*
  "`(sep-by* p sep)` parses zero or more occurrences of `p`, separated by `sep`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/ok nil ts)
    (loop [ts ts
           as [a]]
      (let [[s ts] (jb/attempt ps ts)]
      (if (nil? s)
        (jb/ok as ts)
      (let [[a ts] (jb/run pa ts)]
      (recur ts (conj as a))))))))))

(defn sep-by+
  "`(sep-by+ p sep)` parses one or more occurrences of `p`, separated by `sep`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)]
    (loop [ts ts
           as [a]]
      (let [[s ts] (jb/attempt ps ts)]
      (if (nil? s)
        (jb/ok as ts)
      (let [[a ts] (jb/run pa ts)]
      (recur ts (conj as a)))))))))

(defn skip-sep-by*
  "The parser `(skip-sep-by* p sep)` is an optimized implementation of `(-> (fn [_]) (<$> sep-by* p sep))`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/ok nil ts)
    (loop [ts ts]
      (let [[s ts] (jb/attempt ps ts)]
      (if (nil? s)
        (jb/ok nil ts)
      (let [[_ ts] (jb/run pa ts)]
      (recur ts)))))))))

(defn skip-sep-by+
  "The parser `(skip-sep-by+ p sep)` is an optimized implementation of `(-> (fn [_]) (<$> sep-by+ p sep))`."
  [pa ps]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)]
    (loop [ts ts]
      (let [[s ts] (jb/attempt ps ts)]
      (if (nil? s)
        (jb/ok nil ts)
      (let [[_ ts] (jb/run pa ts)]
      (recur ts))))))))

(defn end-by*
  "The parser `(end-by* p sep)` parses zero or more occurrences of `p` separated and
   ended by `sep`. It returns a vector of the results returned by `p`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/ok nil ts)
    (let [[_ ts] (jb/run ps ts)]
    (loop [ts ts
           as [a]]
      (let [[a ts] (jb/attempt pa ts)]
      (if (nil? a)
        (jb/ok as ts)
      (let [[_ ts] (jb/run ps ts)]
      (recur ts (conj as a)))))))))))

(defn end-by+
  "The parser `(end-by+ p sep)` parses one or more occurrences of `p` separated and
   ended by `sep`. It returns a vector of the results returned by `p`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)
          [_ ts] (jb/run ps ts)]
    (loop [ts ts
           as [a]]
      (let [[a ts] (jb/attempt pa ts)]
      (if (nil? a)
        (jb/ok as ts)
      (let [[_ ts] (jb/run ps ts)]
      (recur ts (conj as a)))))))))

(defn skip-end-by*
  "The parser `(skip-end-by* p sep)` is an optimized implementation of `(-> (fn [_]) (<$> sep-end-by* p sep))`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/ok nil ts)
    (let [[_ ts] (jb/run ps ts)]
    (loop [ts ts]
      (let [[a ts] (jb/attempt pa ts)]
      (if (nil? a)
        (jb/ok nil ts)
      (let [[_ ts] (jb/run ps ts)]
      (recur ts))))))))))

(defn skip-end-by+
  "The parser `(skip-end-by+ p sep)` is an optimized implementation of `(-> (fn [_]) (<$> sep-end-by+ p sep))`."
  [pa ps]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)
          [_ ts] (jb/run ps ts)]
    (loop [ts ts]
      (let [[a ts] (jb/attempt pa ts)]
      (if (nil? a)
        (jb/ok nil ts)
      (let [[_ ts] (jb/run ps ts)]
      (recur ts))))))))

(defn sep-end-by*
  "The parser `(sep-end-by* p sep)` parses zero or more occurrences of `p` separated and
   optionally ended by `sep`. It returns a vector of the results returned by `p`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/ok nil ts)
    (loop [ts ts
           as [a]]
      (let [[s ts] (jb/attempt ps ts)]
      (if (nil? s)
        (jb/ok as ts)
      (let [[a ts] (jb/attempt pa ts)]
      (if (nil? a)
        (jb/ok as ts)
      (recur ts (conj as a)))))))))))

(defn sep-end-by+
  "The parser `(sep-end-by+ p sep)` parses one or more occurrences of `p` separated and
   optionally ended by `sep`. It returns a vector of the results returned by `p`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)]
    (loop [ts ts
           as [a]]
      (let [[s ts] (jb/attempt ps ts)]
      (if (nil? s)
        (jb/ok as ts)
      (let [[a ts] (jb/attempt pa ts)]
      (if (nil? a)
        (jb/ok as ts)
      (recur ts (conj as a))))))))))

(defn skip-sep-end-by*
  "The parser `(skip-sep-end-by* p sep)` is an optimized implementation of `(-> (fn [_]) (<$> sep-end-by* p sep))`."
  [pa ps]
  (fn [ts]
    (let [[a ts] (jb/attempt pa ts)]
    (if (nil? a)
      (jb/ok nil ts)
    (loop [ts ts]
      (let [[s ts] (jb/attempt ps ts)]
      (if (nil? s)
        (jb/ok nil ts)
      (let [[a ts] (jb/attempt pa ts)]
      (if (nil? a)
        (jb/ok nil ts)
      (recur ts))))))))))

(defn skip-sep-end-by+
  "The parser `(skip-sep-end-by+ p sep)` is an optimized implementation of `(-> (fn [_]) (<$> sep-end-by+ p sep))`."
  [pa ps]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)]
    (loop [ts ts]
      (let [[s ts] (jb/attempt ps ts)]
      (if (nil? s)
        (jb/ok nil ts)
      (let [[a ts] (jb/attempt pa ts)]
      (if (nil? a)
        (jb/ok nil ts)
      (recur ts)))))))))

(defn many-till*
  "`(many-till* p end)` applies parser p zero or more times until parser `end` succeeds.
   Returns the list of values returned by `p`."
  [pa pe]
  (fn [ts]
    (loop [ts ts
           as []]
      (let [[e ts] (jb/attempt pe ts)]
      (if-not (nil? e)
        (jb/ok (not-empty as) ts)
      (let [[a ts] (jb/run pa ts)]
      (recur ts (conj as a))))))))

(defn many-till+
  "`(many-till+ p end)` applies parser p one or more times until parser `end` succeeds.
   Returns the list of values returned by `p`."
  [pa pe]
  (fn [ts]
    (let [[a ts] (jb/run pa ts)]
    (loop [ts ts
           as [a]]
      (let [[e ts] (jb/attempt pe ts)]
      (if-not (nil? e)
        (jb/ok as ts)
      (let [[a ts] (jb/run pa ts)]
      (recur ts (conj as a)))))))))

(defn skip-many-till*
  "The parser `(skip-many-till* p sep)` is an optimized implementation of `(-> (fn [_]) (<$> many-till* p sep))`."
  [pa pe]
  (fn [ts]
    (loop [ts ts]
      (let [[e ts] (jb/attempt pe ts)]
      (if-not (nil? e)
        (jb/ok nil ts)
      (let [[_ ts] (jb/run pa ts)]
      (recur ts)))))))

(defn skip-many-till+
  "The parser `(skip-many-till+ p sep)` is an optimized implementation of `(-> (fn [_]) (<$> many-till+ p sep))`."
  [pa pe]
  (fn [ts]
    (let [[_ ts] (jb/run pa ts)]
    (loop [ts ts]
      (let [[e ts] (jb/attempt pe ts)]
      (if-not (nil? e)
        (jb/ok nil ts)
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
(-> (def skip optional) (copy-meta optional))
