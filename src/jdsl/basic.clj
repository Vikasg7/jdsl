(ns jdsl.basic
  (:refer-clojure :exclude [do map peek apply])
  (:require [jdsl.token-stream :as ts :only [peek]]))

;; Success a ts :: (list a ts)
;; Error        :: nil | string
;; Parser a ts  :: ts -> Success a ts | Error

(def error? string?)

(defmacro error
  [& error] `(str ~@error))

(defmacro return
  ([error]  `~error)
  ([val ts] `(vector ~val ~ts)))

(def EOS? nil?)

(defn print-error
  "Prints the message based on original input `ts`, remaining input `ts` and error `msg`."
  [e]
  (let [ex-data (ex-data e)
        ex-msg  (ex-message e)]
  (println ex-msg \newline 
           (:msg ex-data) \newline
           "   Found:" (ts/peek (:ts ex-data)))))

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
      (throw (parse-error result ts))
    (-> result)))))

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

(defn- expand 
  "Expands (a <- parser) or (parser) bindings in the do macro"
  [[sym op prsr :as expr]]
  {:pre [(> (count expr) 0)]}
  (if (= op '<-)
    [[sym 'ts] (list run prsr 'ts)]
  [['_ 'ts] (list run expr 'ts)]))

(defmacro do
  "Haskel like do macro to abstract away passing around `ts` and calling `run` function."
  [& exprs]
  (let [bindings (mapcat expand exprs)]
  `(fn [~'ts]
    (let [~@bindings]
    (list ~'_ ~'ts)))))
