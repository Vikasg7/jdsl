(ns jdsl.combinator-flip
  "This namespace creates and holds version of jdsl.combinator functions (marked <Flipable> in doc-string) 
   with parameters in reverse orders. This might come in handy while extending parses with -> and ->> 
   (thread macros). So when you require the jdsl.combinator-flip namespace, you should be able to access functions 
   in jsdl.combinators (marked <Flipable> in doc-string) with their arguments flipped i.e. in reverse order."
  (:refer-clojure :exclude [peek map apply])
  (:require [clojure.string :as str]))

(defn- flip-args
  "Flips arguments for f with arity 2 or 3"
  [f]
  (let [args (first (:arglists (meta f)))]
  (case (count args)
    2 (fn [ab aa] (f aa ab))
    3 (fn [aa ac ab] (f ab aa ac))
      nil)))

(defn- copy-doc-string
  "Copies over doc string from other #'var"
  [from-var to-var]
  (let [new-doc (str "**NOTE**: *Read following docs with flipped parameters in mind.*  " \newline
                     (:doc (meta from-var)))]
  (alter-meta! to-var update-in [:doc] (constantly new-doc))))

(defn- copy-symbols-with-args-flipped
  "Copies intern symbols from `from-ns` namepace to `to-ns` namespace."
  [from-ns to-ns]
  (let [interns (ns-publics from-ns)]
  (doseq [[sym f] interns]
    (when (str/includes? (:doc (meta f)) "`<Flipable>`")
    (when-let [nf (flip-args f)]
      (->> (intern to-ns sym nf)
           (copy-doc-string f)))))))

(copy-symbols-with-args-flipped 'jdsl.combinator *ns*)
