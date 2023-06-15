(ns jdsl.core
  (:require [jdsl.basic        :as jb]
            [jdsl.combinator   :as jc]
            [jdsl.combinator-flip      :as jf]
            [jdsl.char-parser  :as jp]
            [jdsl.char-stream  :as cs]))

(defn -main
  [& args]
  (println (str "Thanks for using JDSL. " \newline
                "Use `bb test` or `lein test` for running JDSL tests.")))
