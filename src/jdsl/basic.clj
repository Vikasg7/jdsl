(ns jdsl.basic
  (:refer-clojure :exclude [do map peek apply])
  (:require [jdsl.char-stream :as cs]))

;; Ok a ts     :: [a ts]
;; Error       :: nil | string
;; Result a ts :: Ok a ts | Error 
;; Parser a ts :: ts -> Result a ts

(def error? (some-fn nil? string?))

(defmacro error
  ([error] `~error)
  ([expect & error] `(str ~expect ~@error)))

(defmacro ok
  [parsed ts] `(vector ~parsed ~ts))

(def parsed       first)
(def token-stream second)
(def ts second) ;; short for token-stream

(def EOS? nil?)

(defn print-error
  "Prints the parse-error. It receives `e` which is an instance of
   `clojure.lang.ExceptionInfo` which contains message, :msg and
   :ts and can be accessed with (ex-message e), (:msg (ex-data e)),
   (:ts (ex-data e))."
  [e]
  (let [ex-data (ex-data e)
        ex-msg  (ex-message e)]
  (println ex-msg \newline 
           (or (:msg ex-data) "Expected: ") \newline
           "   Found:" (cs/peek (:ts ex-data)))))

(defn parse-error
  "Helper function to generate parsing error"
  [msg ts]
  (ex-info "ParseError" {:ts ts :msg msg}))

(defn run
  "Runs the parser `p` on input `ts`, throws parsing error.
   Parse Error contains following fields which can be accessed
   using `ex-message` and `ex-data` function.  

   ```clojure
   (try
     (run parser (cs/create \"abc\"))
   (catch clojure.lang.ExceptionInfo e
     ;; always equal to \"ParseError\"
     (println (ex-message e))
     ;; all messages/labels attached to the parsers
     ;; using <$> or by simply returing jb/error from
     ;; the parsers.
     (println (:msg (ex-data e)))
     ;; Current state of the token stream (ie. char stream)
     ;; when the error happened.
     (println (:ts (ex-message e)))))
   ```  
   You can use all of above information to create your own
   custom error print function."
  ([p]
    (fn [ts]
      (run p ts)))
  ([p ts]
    (let [result (p ts)]
    (if (error? result)
      (throw (parse-error result ts))
    (-> result)))))

(defmacro ignore-parse-error 
  "Macro to catch-ignore only parse errors and throw rest of them."
  ^:private
  [& body]
  `(try 
     ~@body
   (catch clojure.lang.ExceptionInfo ~'e
     (when (not= "ParseError" (ex-message ~'e))
       (throw ~'e)))))

(defn attempt
  "Attempts to run parser `p`, backtracks if fails AND no input is consumed.
   It compares the position of `ts` returned as part of `(ex-data e)` with original 
   position of `ts` passed as an argument to `attempt` function to decide whether
   input is consumed or not."
  [p ts]
  (try
    (run p ts)
  (catch clojure.lang.ExceptionInfo e
    (if (not= "ParseError" (ex-message e))
      (throw e)
    (let [cs  (:ts (ex-data e))]
    (if (not= (cs/position ts) (cs/position cs))
      (throw e)
    (vector nil ts)))))))

(defn- expand 
  "Expands (a <- parser) or (parser) bindings in the do macro."
  [form]
  (if-not (list? form)
    [['_  'ts] (list run form 'ts)]
  (if (= 1 (count form))
    [['_  'ts] (list run (first form) 'ts)]
  (let [[sym op prsr] form]
  (if (= '<- op)  
    [[sym 'ts] (list run prsr 'ts)]
  [['_  'ts] (list run form 'ts)])))))

(defmacro do
  "Haskel like do macro to abstract away passing around `ts` and calling `run` function.  
   ```clojure
   (ns example
    (:require [jdsl.basic       :as jb]
              [jdsl.char-parser :as jp]))
   (def parser
    (jb/do
      jp/any-char
      (jp/any-char)
      (jp/char \\a)
      (a <- (jc/char \\a))
      (b <- jc/any-char)
      (_ <- jc/any-char)
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
      (jb/run (jc/return [a b] ts)))))
    ``` 
   *Note*:  
   - `ts` is the token stream (i.e. char-stream in context of jdsl library)
   - `ts` from the previous `jb/run` call is passed to the next call.
   - `_` means parsed value is ignored."
  [& exprs]
  (let [bindings (mapcat expand (butlast exprs))
        ret-form (list run (last exprs) 'ts)]
  `(fn [~'ts]
    (let [~@bindings]
    (~@ret-form)))))
