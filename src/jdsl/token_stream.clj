(ns jdsl.token-stream
  (:refer-clojure :exclude [next peek]))

(defn create
  "Creates new token-stream for parsers to work with.   
   The `tokens` passed as an argument to the `create`  
   **MUST** implement [clojure.lang.indexed](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Indexed.java)
   and [clojure.lang.ISeq](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/ISeq.java)  
   interfaces."
  [tokens]
  [tokens #_position= -1])

(def tokens   first)
(def position second)

(defn next
  "Returns the next token in the token stream if available else nil.  
   It also returns updated token-stream. It doesn't mutate incoming  
   token-stream state coz Mutation kills BackTracking."
  [[tokens position]]
  (when-let [next-token (first tokens)]
    (-> [next-token [tokens (inc position)]])))

(defn state
  "A parser that returns the current token-stream state"
  [ts]
  (-> [ts ts]))

(defn peek
  "`[[tokens :as _ts]]]`  
   Peeks next token from the stream and returns it. Doesn't change the `ts` state.  

   `[[tokens] token]`  
   Peeks `n` chars in a char-stream and returns them, if it hits End of Tokens before  
   `n` chars are read, it will return available chars. Assumes token-stream is a char-stream."
  ([[tokens :as _ts]]
    (when-let [next-token (first tokens)]
      (-> next-token)))
  ([[tokens :as _ts] n]
    (take n tokens)))

(defn peek-nth
  "Return the next token at (current postion + offset) in the current stream."
  [[tokens position :as _ts] offset]
  (when-let [token (nth tokens (+ position offset) nil)]
    (-> token)))

(defn match
  "Returns true if the next token in the stream matches the specified `token`."
  [[tokens] token]
  (= token (first tokens)))

(defn match-str
  "Returns ture if the next token in the stream matches the specified `str`.  
   Works only with char streams like string"
  [[tokens :as _ts] str]
  (= (seq str) (take (count str) tokens)))

(defn skip
  "[[tokens position :as ts]]  
   Advances the position within the stream by 1 char,   
   except at the end of the stream, where it returns nil.  
   
   [ts offset]  
   Advances the position by offset, except at the end of the stream,  
   where it returns nil."
  ([[tokens position :as _ts]]
    (when (first tokens)
      (-> [tokens (inc position)])))
  ([ts offset]
    (when (peek-nth ts offset)
      (-> [tokens (+ offset position)]))))

(defn skip-newline
  "Skips the new line character if next character is a \\r, \\n, \\r\\n."
  [ts]
  (let [[r n :as rn] (peek ts 2)]
  (if (= rn [\return \newline])
    (skip ts 2)
  (when (or (= r \return)
            (= n \newline))
    (skip ts 1)))))
