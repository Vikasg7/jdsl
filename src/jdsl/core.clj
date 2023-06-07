(ns jdsl.core
  (:require [jdsl.basic        :as jb]
            [jdsl.combinator   :as jc]
            [jdsl.flip         :as jf]
            [jdsl.parser       :as jp]
            [jdsl.token-stream :as ts]))

(defn -main
  [& args]
  (let [ab (jf/label "Expected: ab" (jf/<> (jp/char \a) (jp/char \b)))]
  (try
    (println (jb/run ab (ts/create "bac")))
  (catch clojure.lang.ExceptionInfo e
    (jb/print-error e)))))
