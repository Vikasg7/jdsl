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
  [val ts] `(vector ~val ~ts))

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
  "[p]  
   Returs a parser that attempts parser `p`, returns nil as result, if fails.
   It doesn't affect `ts` state in case of failure.  
   
   [p ts]  
   Attempts to run parser `p`, backtracks if fails."
  ([p]
    (fn [ts] 
      (attempt p ts)))
  ([p ts]
    (if-let [result (ignore-parse-error (run p ts))]
      (-> result)
    (vector nil ts))))

(defn- expand 
  "Expands (a <- parser) or (parser) bindings in the do macro"
  [expr]
  (let [[sym op prsr] (when (list? expr) expr)]
  (cond (= op '<-) [[sym 'ts] (list run prsr 'ts)]
        :else      [['_  'ts] (list run expr 'ts)])))

(defmacro do
  "Haskel like do macro to abstract away passing around `ts` and calling `run` function."
  [& exprs]
  (let [bindings (mapcat expand (butlast exprs))
        ret-form (list run (last exprs) 'ts)]
  `(fn [~'ts]
    (let [~@bindings]
    (~@ret-form)))))
