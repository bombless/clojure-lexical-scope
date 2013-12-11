(ns lexical-scope.core)

(defn transform
  "transform item from (define a) to [a nil]"
  [[_ name & [value]]]
  `[~name ~value])
(defmacro cdddr [item] `(next (next (next ~item))))
(defn predicate
  "determin if we met a (define blahblah~~)"
  [item]
  (and (list? item)
       (= 'define (first item))
       ;; (not (nil? (second item)))
       ; Cause nil is not a symbol in Clojure
       ; But, that is not true in Common Lisp
       (symbol? (second item))
       ;which means there's only 3 elements or less
       (= nil (cdddr item))))

(defn reshape-recur
  [input]
  (if (and
       (not= '() input)
       (seq? input))
    (if (predicate (first input))
      (list
       nil
       `(~'let ~(transform (first input))
          ~@(reshape-recur (rest input))))
      (cons
       (reshape-recur (first input))
       (reshape-recur (rest input))))
    input))
(defmacro lexical-do
  [& body]
  (cons 'do (reshape-recur body)))
;; (defmacro p
;;   [& body]
;;   (= body '((define a) (print a))))
;; (defmacro l
;;   [& body]
;;   (cons 'do (reshape-recur '((define a) (print a)))))
;; (defn foo []
;;   (l)
;;   (print " *good!* ")
;;   (if (p (define a) (print a))
;;     (println " *good* ")
;;     (println " *bad* ")))
;; (defn bar []
;;   (lexical-do (define a) (print a)))
;; (defmacro listp [& body]
;;   (vector? body))
