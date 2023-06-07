(ns jdsl.flip
  "The version of jdsl.combinator functions with parameters in reverse orders.  
   This might come in handy while extending parses with -> and ->> (thread macros)."
  (:refer-clojure :exclude [peek map apply]))

(defn- flip-args
  [f]
  (let [args (first (:arglists (meta f)))]
  (case (count args)
    2 (fn [aa ab] (f ab aa))
    3 (fn [aa ab ac] (f ab ac aa))
      nil)))

(defn- copy-doc-string
  "Copies over doc string from other #'var"
  [from-var to-var]
  (let [new-doc (str "**NOTE**: Version with flipped parameters.  " \newline
                     (:doc (meta from-var)))]
  (alter-meta! to-var update-in [:doc] (constantly new-doc))))

(defn- copy-symbols-with-args-flipped
  "Copies intern symbols from `from-ns` namepace to `to-ns` namespace."
  [from-ns to-ns]
  (let [interns (ns-publics from-ns)]
  (doseq [[sym f] interns]
    (when-let [nf (flip-args f)]
      (->> (intern to-ns sym nf)
           (copy-doc-string f))))))

(copy-symbols-with-args-flipped 'jdsl.combinator *ns*)
