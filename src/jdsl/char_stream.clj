(ns jdsl.char-stream
  "`jdsl.char-stream` contains functions to create char stream and functions to
   read/peek/match characters in char stream."
  (:refer-clojure :exclude [next peek read]))

(defn create
  "Creates new char-stream for parsers to work with."
  [string]
  (let [buf (when-not (empty? string) 
              (vec string)) 
        pos -1              
        end (count string)]
  [buf pos end]))

(def buf first)
(def pos second)
(def end #(nth % 2))
(def EOS? nil?)

(defn next
  "Returns the next char in the char-stream if available else nil.
   It also returns updated char-stream. It doesn't mutate incoming
   char-stream state coz Mutation kills BackTracking."
  [cs]
  (let [[buf pos end] cs
        npos          (inc pos)]
  (when (< npos end)
    (-> [(nth buf npos) [buf npos end]]))))

(def state
  "A parser that returns the current char-stream state."
  (fn [cs]
    (-> [cs cs])))

(defn peek
  "`[cs]`
   Peeks next char from the stream and returns it. Doesn't change the `cs` state.
   
   `[cs n]`
   Peek next n chars from the stream and returns them."
  ([cs]
    (let [[buf pos end] cs
          npos          (inc pos)]
    (when (< npos end)
      (nth buf npos))))
  ([cs n]
    (let [[buf pos end] cs
          npos          (+ pos n)]
    (when (some? buf)      
      (if (< npos end)
        (subvec buf (inc pos) (inc npos))
      (subvec buf (inc pos)))))))

(defn peek-nth
  "Return the next char at (current postion + offset) in the char-stream."
  [cs offset]
  (let [[buf pos end] cs
        npos          (+ pos offset)]
  (when (< npos end)
    (nth buf npos))))

(defn match
  "Returns true if the next char in the stream matches the specified `c`."
  [cs c]
  (let [[buf pos end] cs
        npos          (inc pos)]
  (and (< npos end)
       (= c (nth buf npos)))))

(defn match-seq
  "Returns true if the next char in the stream matches the specified sequence `s`."
  [cs s]
  (= s (peek cs (count s))))

(defn match-str
  "Returns true if the next char in the stream matches the specified `string`."
  [cs string]
  (= (seq string) (peek cs (count string))))

(defn skip
  "[cs]
   Advances the position within the stream by 1 char, 
   except at the end of the stream, where it returns nil.
   
   [cs offset]
   Advances the position by offset, except at the end of the stream,
   where it returns nil."
  ([cs]
    (skip cs 1))
  ([cs offset]
    (let [[buf pos end] cs
          npos          (+ pos offset)]
    (when (< npos end)
      (-> [buf npos end])))))

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

   [cs n]
   Reads next `n` chars"
  ([cs]
    (when-let [[r n] (peek cs 2)]
      (if (and (= r \return) (= n \newline))
        [\newline (skip cs 2)]
      (if (or (= r \return) (= r \newline))
        [\newline (skip cs 1)]
      (when-not (EOS? r) ;; r can be nil denoting EOS
        [r (skip cs 1)])))))
  ([cs n]
    (loop [nn   n
           acc []
           cs  cs]
      (if (zero? nn)
        (if (= n 1) 
          [(first acc) cs]
        [(not-empty acc) cs])
      (if-let [[c cs] (read cs)]
        (recur (dec nn) (conj acc c) cs)
      [(not-empty acc) cs])))))

(defn read-char
  "Reads the next n chars"
  [cs c]
  (if (or (= c \return) (= c \newline))
    (when-let [cs (skip-newline cs)]
      [\newline cs])
  (when (= c (peek cs))
    [c (skip cs 1)])))
