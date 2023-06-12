(ns jdsl.char-stream
  (:refer-clojure :exclude [next peek read]))

(defn create
  "Creates new char-stream for parsers to work with."
  [string]
  [(when-not (empty? string) (vec string)) 
   #_position= -1])

(defn next
  "Returns the next char in the char-stream if available else nil.  
   It also returns updated char-stream. It doesn't mutate incoming  
   char-stream state coz Mutation kills BackTracking."
  [cs]
  (let [[string position] cs]
  (when-let [next-char (nth string (inc position) nil)]
    (-> [next-char [string (inc position)]]))))

(def state
  "A parser that returns the current char-stream state."
  (fn [cs]
    (-> [cs cs])))

(defn peek
  "`[cs]`  
   Peeks next char from the stream and returns it. Doesn't change the `cs` state.  
   
   `[cs n]  
   Peek next n chars from the stream and returns them.`  
   "
  ([cs]
    (let [[string position] cs]
    (nth string (inc position) nil)))
  ([cs n]
    (let [[string position] cs]
    (try
      (subvec string (inc position) (+ n 1 position))
    (catch IndexOutOfBoundsException _
      (subvec string (inc position)))
    (catch NullPointerException _)))))

(defn peek-nth
  "Return the next char at (current postion + offset) in the char-stream."
  [cs offset]
  (let [[string position] cs]
  (nth string (+ position offset) nil)))

(defn match
  "Returns true if the next char in the stream matches the specified `c`."
  [cs c]
  (let [[string position] cs]
  (= c (nth string (inc position) nil))))

(defn match-seq
  "Returns true if the next char in the stream matches the specified sequence `s`."
  [cs s]
  (= s (peek cs (count s))))

(defn match-str
  "Returns true if the next char in the stream matches the specified `string`."
  [cs string]
  (= (seq string) (peek cs (count string))))

(defn skip
  "[[string position :as cs]]  
   Advances the position within the stream by 1 char,   
   except at the end of the stream, where it returns nil.  
   
   [cs offset]  
   Advances the position by offset, except at the end of the stream,  
   where it returns nil."
  ([cs]
    (let [[string position] cs]
    (when (nth string (inc position) nil)
      (-> [string (inc position)]))))
  ([[string position :as cs] offset]
    (when (peek-nth cs offset)
      (-> [string (+ offset position)]))))

(defn skip-newline
  "Skips the new line character if next character is a \\r, \\n, \\r\\n."
  [cs]
  (let [[r n] (peek cs 2)]
  (if (and (= r \return) (= n \newline))
    (skip cs 2)
  (when (or (= r \return) (= r \newline))
    (skip cs 1)))))

(defn skip-char
  "Skips next char on the stream if it matches specified `c`"
  [cs c]
  (when (= c (peek cs))
    (skip cs 1)))

(defn read
  "[cs]  
   Reads the next char, also takes care of \\r\\n, \\r, \\n  

   [cs c]
   Reads the next char if matches the specified `c`"
  ([cs]
    (when-let [[r n] (peek cs 2)]
      (if (and (= r \return) (= n \newline))
        [\newline (skip cs 2)]
      (if (or (= r \return) (= r \newline))
        [\newline (skip cs 1)]
      [r (skip cs 1)]))))
  ([cs c]
    (if (or (= c \return) (= c \newline))
    (when-let [cs (skip-newline cs)]
      [\newline cs])
    (when (= c (peek cs))
      [c (skip cs 1)]))))
