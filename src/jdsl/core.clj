(ns jdsl.core
  {:clj-kondo/ignore [:unused-namespace]}
  (:require [jdsl.basic        :as jb]
            [jdsl.combinator   :as jc]
            [jdsl.char-parser  :as jp]
            [jdsl.char-stream  :as cs]))

#_{:clj-kondo/ignore [:unused-binding]}
(defn -main
  [& args]
  (println (str "Thanks for using JDSL. " \newline
                "Use `bb test` or `lein test` for running JDSL tests.")))
