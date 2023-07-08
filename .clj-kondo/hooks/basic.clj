(ns hooks.basic
  (:refer-clojure :exclude [do])
  (:require [clj-kondo.hooks-api :as api]
            [jdsl.basic :as jb :refer [run]]))

(defn- expand->bindings 
  "Expands `parser`, `(parser)`, `(a <- parser)`, `(parser args)` forms 
   in the do macro to `let` bindings."
  [form]
  (if-not (list? form)
    [['_ 'ts] (list run form 'ts)]
  (if (= 1 (count form))
    [['_ 'ts] (list run (first form) 'ts)]
  (let [[sym op prsr] form]
  (if (= '<- op)  
    [[sym 'ts] (list run prsr 'ts)]
  [['_ 'ts] (list run form 'ts)])))))

(defn- expand->body
  "Expands the `parser`, `(parser)`, `(parser args)` form 
   in the do macro to `let` body"
  [form]
  (if-not (list? form)
    (list run form 'ts)
  (if (= 1 (count form))
    (list run (first form) 'ts)
  (list run form 'ts))))

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

(def macro
  "(jb/do
      jp/any-char
      (jp/any-char)
      (jp/char \\a)
      (a <- (jp/char \\a))
      (b <- jp/any-char)
      (_ <- jp/any-char)
      (jc/return [a b]))")

;; following print statement works and prints the exact same api/list-node as the 
;; analyze-hook would print as I tested earlier.
(println (api/macroexpand #'hooks.basic/do (api/parse-string macro) {}))
;; prints following list-node as printed by analyze-hook approach
;; <list: (clojure.core/fn [ts] (clojure.core/let [[_ ts] (#function[jdsl.basic/run] jp/any-char ts) [_ ts] (#function[jdsl.basic/run] jp/any-char ts) [_ ts] (#function[jdsl.basic/run] (jp/char \a) ts) [a ts] (#function[jdsl.basic/run] (jp/char \a) ts) [b ts] (#function[jdsl.basic/run] jp/any-char ts) [_ ts] (#function[jdsl.basic/run] jp/any-char ts)] (#function[jdsl.basic/run] (jc/return [a b]) ts)))>

;; Analysis hook approach

;; (defn expand->bindings [form]
;;   (let [run (api/token-node jb/run)
;;         ts  (api/token-node 'ts)
;;         _   (api/token-node '_)]
;;   (if-not (api/list-node? form)
;;     [(api/vector-node [_ ts]) (api/list-node (list run form ts))]
;;   (if (= 1 (count (:children form)))
;;     [(api/vector-node [_ ts]) (api/list-node (list run (first (:children form)) ts))]
;;   (let [[sym op prsr] (:children form)]
;;   (if (= '<- (:value op))
;;     [(api/vector-node [sym ts]) (api/list-node (list run prsr ts))]
;;   [(api/vector-node [_ ts]) (api/list-node (list run form ts))]))))))

;; (defn expand->body [form]
;;   (let [run (api/token-node jb/run)
;;         ts  (api/token-node 'ts)]
;;   (if-not (api/list-node? form)
;;     (list run form ts)
;;   (if (= 1 (count (:children form)))
;;     (list run (first (:children form)) ts)
;;   (list run form ts)))))

;; (defn do- [{:keys [node]}]
;;   (let [fn-token  (api/token-node 'fn)
;;         fn-params (api/vector-node [(api/token-node 'ts)])
;;         let-token (api/token-node 'let)
;;         let-binds (api/vector-node (vec (mapcat expand->bindings (butlast (rest (:children node))))))
;;         let-body  (api/list-node (expand->body (last (:children node))))
;;         fn-body   (api/list-node (list let-token let-binds let-body))]
;;   {:node (api/list-node (list fn-token fn-params fn-body))}))

;; (def macro
;;   "(jb/do
;;       jp/any-char
;;       (jp/any-char)
;;       (jp/char \\a)
;;       (a <- (jp/char \\a))
;;       (b <- jp/any-char)
;;       (_ <- jp/any-char)
;;       (jc/return [a b]))")

;; (println (:node (do- {:node (api/parse-string macro)})))