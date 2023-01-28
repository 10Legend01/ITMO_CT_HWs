(def constant constantly)
(defn variable [name] (fn [vars] (vars name)))

(defn makeOperation [op] (fn [& args] (fn [arg] (apply op (mapv (fn [f] (f arg)) args)))))

(def add (makeOperation +))
(def subtract (makeOperation -))
(def multiply (makeOperation *))
(def divide (makeOperation (fn
                               ([first] (/ 1.0 (double first)))
                               ([first & rest] (reduce (fn [a, b] (/ a (double b))) (double first) rest)))))
(def negate subtract)
(def sum add)
(def avg (makeOperation (fn [& args] (/ (double (apply + args)) (count args)))))

(def argToOperation
  {'+ add,
   '- subtract,
   '* multiply,
   '/ divide,
   'negate negate,
   'sum sum,
   'avg avg})

(defn parseFunction [expression]
  (letfn [(parse [arg]
            (cond
              (list? arg) (apply (argToOperation (first arg)) (mapv parse (rest arg)))
              (number? arg) (constant arg)
              :else (variable (str arg))))]
    (parse (read-string expression))))
