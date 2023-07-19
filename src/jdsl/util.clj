(ns jdsl.util)

(defn alpha-num? [c] 
  (or (<= (int \A) (int c) (int \Z))
      (<= (int \a) (int c) (int \z))
      (<= (int \0) (int c) (int \9))))

(defn hex? [c]
  (or (<= (int \0) (int c) (int \9))
      (<= (int \a) (int c) (int \f))
      (<= (int \A) (int c) (int \F))))

(defn digit? [c] 
  (<= (int \0) (int c) (int \9)))

(defn alphabet? [c] 
  (or (<= (int \A) (int c) (int \Z))
      (<= (int \a) (int c) (int \z))))

(defn upper? [c] 
  (<= (int \A) (int c) (int \Z)))

(defn lower? [c] 
  (<= (int \a) (int c) (int \z)))

(defn octal? [c] 
  (<= (int \0) (int c) (int \7)))

(def space? (set "\r\n\t \f"))
