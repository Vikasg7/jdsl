(ns hooks.basic
  (:refer-clojure :exclude [do])
  (:require [jdsl.basic :as-alias jb]))

(defn- expand->bindings 
  "Expands `parser`, `(parser)`, `(a <- parser)`, `(parser args)` forms 
   in the do macro to `let` bindings."
  [form]
  (if-not (list? form)
    [['_ 'ts] (list `jb/run form 'ts)]
  (if (= 1 (count form))
    [['_ 'ts] (list `jb/run (first form) 'ts)]
  (let [[sym op prsr] form]
  (if (= '<- op)  
    [[sym 'ts] (list `jb/run prsr 'ts)]
  [['_ 'ts] (list `jb/run form 'ts)])))))

(defn- expand->body
  "Expands the `parser`, `(parser)`, `(parser args)` form 
   in the do macro to `let` body"
  [form]
  (if-not (list? form)
    (list `jb/run form 'ts)
  (if (= 1 (count form))
    (list `jb/run (first form) 'ts)
  (list `jb/run form 'ts))))

(defmacro do
  "Haskel like do macro to abstract away passing around `ts` and calling `run` function.  
   ```clojure
   (ns example
    (:require [jdsl.basic       :as jb]
              [jdsl.char-parser :as jp]
              [jdsl.combinator  :as jc]))
   (def parser
    (jb/do
      jp/any-char
      (jp/any-char)
      (jp/char \\a)
      (a <- (jp/char \\a))
      (b <- jp/any-char)
      (_ <- jp/any-char)
      (jc/return [a b])))
   ```  
   generates following code (in order):  
   ```clojure
   (def parser
    (fn [ts]
      (let [[_ ts] (jb/run jp/any-char ts)
            [_ ts] (jb/run jp/any-char ts) ;; Yes, jp/any-char and (jb/any-char) are same coz they take zero args.
            [_ ts] (jb/run (jp/char \\a) ts)
            [a ts] (jb/run (jp/char \\a) ts)
            [b ts] (jb/run jp/any-char ts)
            [_ ts] (jb/run jb/any-char ts)]
      (jb/run (jc/return [a b]) ts))))
    ``` 
   *Note*:  
   - `ts` is the token stream (i.e. char-stream in context of jdsl library)
   - `ts` from the previous `jb/run` call is passed to the next call.
   - `_` means parsed value is ignored."
  [& exprs]
  (let [bindings (mapcat expand->bindings (butlast exprs))
        body     (expand->body (last exprs))]
  `(fn [~'ts]
    (let [~@bindings]
    (~@body)))))
