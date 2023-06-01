(ns jdsl.core)

;; Success a ts :: Pair a ts
;; Error        :: nil | string
;; Parser a ts  :: ts -> ParserSuccess a ts | Error

(defn parse-error
  "helper function to generate error"
  [msg rest-ts]
  (ex-info (str "ParseError " msg) {:rest-ts rest-ts}))

(defn run
  "runs the parser p on input ts, throws parsing error"
  ([p ts]
    (run p ts ""))
  ([p ts msg]
    (let [result (p ts)]
    (if (or (nil?    result)
            (string? result))
      (-> result)
    (throw (parse-error (or msg result) ts))))))

(defmacro try-catch-all
  "try to execute the body, returns nil if fails"
  {:private true}
  [& body]
  `(try ~@body (catch Exception ~'_)))

(defn- attempt
  "attempt to run the p on input ts, backtracks if fails"
  [p ts]
  (if-let [result (try (run p ts) (catch Exception _))]
    (-> result)
  (list nil ts)))

(defn print-error
  "prints the error thrown by run function"
  [ts rest-ts]
  :todo)

(defn <$>
  "(<$>) :: (a -> b) -> p a -> p b"
  [f p]
  (fn [ts]
    (when-let [[a ts] (run p ts)]
      (list (f a) ts))))

(defn <$
  "(<$) :: a -> p b -> p a"
  [a p]
  (fn [ts]
    (when-let [[_ ts] (run p ts)]
      (list a ts))))

(defn $>
  "($>) :: p a -> b -> p b"
  [p b]
  (fn [ts]
    (when-let [[_ ts] (run p ts)]
      (list b ts))))

(defn return
  "return :: a -> p a"
  [a]
  (fn [ts]
    (list a ts)))

(defn zero
  "parser that always lists a zero value i.e. ParserError = nil"
  [_] nil)

(defn fail
  "parser that always lists a zero value i.e. ParserError = str"
  [msg]
  (fn [_] msg))

(defn lift
  "lift :: (a -> b) -> (a -> p b)"
  [f]
  (fn [a]
    (return (f a))))

(defn >>=
  "(>>=) :: p a -> (a -> p b) -> p b"
  [p f]
  (fn [ts]
    (when-let [[a ts] (run p ts)]
    (when-let [[b ts] (run (f a) ts)]
      (list b ts)))))

(defn <>
  "(<>) :: Monoid p => p a -> p b -> p [a b]  
   mappend"
  [p1 p2]
  (fn [ts]
    (when-let [[a ts] (run p1 ts)]
    (when-let [[b ts] (run p2 ts)]
      (list [a b] ts)))))

(defn <?>
  "(<?>) :: p a -> string -> p a"
  [p msg]
  (fn [ts]
    (when-let [[a ts] (run p ts msg)]
      (list a ts))))

;; (try (throw (Exception. "hello")) (catch Exception _))

(def bind "alias for >>=" >>=)

(defn merge-bind [body bind]
  (if (and (not= symbol? bind)
           (= 3 (count bind))
           (= '<- (second bind)))
    `(>>= ~(last bind) (fn [~(first bind)] ~body))
    `(>>= ~bind (fn [~'_] ~body))))

(defmacro do* [& forms]
  (reduce merge-bind (reverse forms)))


(defn plus [parser]
  (do*
   (a  <- parser)
   (as <- (many parser))
   (return (cons a as))))

(defn many [] )
(defn parser [] )

(def myform '(do*
              (a <- parser)
              (as <- (many parser))
              (return (cons a as))))

(def myform2 '(do*
                (string "clojure")
                (match " ")
                (major <- digit)
                (match ".")
                (minor <- digit)
                (return (str "major: " major "; minor: " minor))))

(macroexpand myform2)

(macroexpand myform)

(>>= parser (fn [a] (>>= (many parser) (fn [as] (return (cons a as))))))

(>>= parser (fn [a] (>>= (many parser) (fn [as] (return (cons a as))))))

(>>= parser (fn [a] (>>=
    (many parser) (fn [as]
    (return (cons a as))))))

(>>=
 (string "clojure") (fn [_] (>>=
 (match " ") (fn [_] (>>=
  digit (fn [major] (>>=
 (match ".") (fn [_] (>>=
  digit (fn [minor]
 (return (str "major: " major "; minor: " minor))))))))))))