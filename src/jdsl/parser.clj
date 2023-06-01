(ns jdsl.parser
  (:require [clojure.string :refer [starts-with?]]))

(defn sat
  "parse the first character if statisfies the pred"
  [pred] 
  (fn [[fst & rst]]
    (when (and fst (pred fst))
      (list fst rst))))

(defn chr
  "parse the first character if matches the ch"
  [ch] (sat (partial = ch)))

(defn txt
  "parse the given txt"
  [txt]
  (fn [input]
    (when (starts-with? input txt)
      (let [rst (subs input (count txt))]
      (list txt rst)))))

