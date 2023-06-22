(ns jdsl.core
  (:require [jdsl.basic        :as jb]
            [jdsl.combinator   :as jc]
            [jdsl.char-parser  :as jp]
            [jdsl.char-stream  :as cs]
            [jdsl.combinator-flip :as jf]))

(defn -main
  [& args]
  (println (str "Thanks for using JDSL. " \newline
                "Use `bb test` or `lein test` for running JDSL tests.")))
