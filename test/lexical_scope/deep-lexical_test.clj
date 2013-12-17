(ns lexical-scope.core-test
  (:require [clojure.test :refer :all]
            [lexical-scope.deep-lexical :refer :all]))

(deftest major-test
  (testing "our whole lexical things"
    (is
     (=
      (lexical-do
       (let [counter
             ((fn []
                (define c 0)
                (fn []
                  (setq c (+ c 1))
                  c)))]
         (counter)
         (counter)
         (counter)))
      3))
    (is
     (=
      (lexical-do
       (define a 1)
       (let [b (+ a 1)
             a 3]
         (* a b)
         ))
      6))))
