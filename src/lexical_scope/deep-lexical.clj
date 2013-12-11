(ns lexical-scope.deep-lexical)

(defn transform-define
  "transform item from (define a) to (def <some symbol> nil)"
  [sym value-handler [_ _ & [value]]]
  `(def ~sym (~'atom ~(value-handler value))))

(defn transform-setq
  "transform item from (setq a 1) to (reset! a 1)"
  [sym value-handler [_ _ & [value]]]
  `(~'reset! ~sym ~(value-handler value)))


(defmacro cdddr [item] `(next (next (next ~item))))

(defn predicate-define
  "determin if we met a (define blahblah~~)"
  [item]
  (if (and (seq? item)
           (= 'define (first item))
           (symbol? (second item))
       ;which means there's only 3 elements or less
           (= nil (cdddr item)))
    (second item)))

(defn predicate-setq
  "determin if we met a (setq blah blah~~)"
  [item]
  (if (and (seq? item)
           (= 'setq (first item))
           (symbol? (second item))
           (= nil (cdddr item)))
    (second item)))


(defn reshape-recur
  [vars input]
  (if (and
       (not= '() input)
       (or (vector? input) (seq? input)))
    (let [new-name (predicate-define (first input))
          old-name (predicate-setq (first input))]
      (cond
       new-name
       (let [sym (gensym)
             new-vars (assoc vars new-name sym)]
         (cons (transform-define
                sym
                #(reshape-recur vars %)
                (first input))
               (reshape-recur new-vars (rest input)))),
       old-name
       (cons (transform-setq
              (get vars old-name)
              #(reshape-recur vars %)
              (first input))
              (reshape-recur vars (rest input))),
       true
       (((fn [] (if (vector? input) #(apply vector (cons %1 %2)) cons)))
        (reshape-recur vars (first input))
        (reshape-recur vars (rest input)))))
    (if-let [sub (get vars input)]
      `(~'deref ~sub)
      input)))


(defmacro lexical-do
  [& body]
  `(do ~@(reshape-recur {} body)))

(def foo (macroexpand-1
          '(lexical-do
            (let [counter
                  ((fn []
                     (define c 0)
                     (fn []
                       (setq c (+ c 1))
                       c)))]))))
(def bar '(lexical-do
            (let [counter
                  ((fn []
                     (define c 0)
                     (fn []
                       (setq c (+ c 1))
                       c)))])))
;; (lexical-do
;;  (let [counter
;;        ((fn []
;;           (define c 0)
;;           (fn []
;;             (setq c (+ c 1))
;;             c)))]
;;    (counter)
;;    (counter)
;;    (counter)))
