(ns lexical-scope.core-test
  (:require [clojure.test :refer :all]
            [lexical-scope.core :refer :all]))

(deftest test-predicate
  (testing "test inputs that should pass the test"
    (is (true? (predicate '(define a))))
    (is (true? (predicate '(define a nil))))
    (is (true? (predicate '(define foo-bar)))))
  (testing "test inputs that should not pass the test"
    (is (false? (predicate '(define 'b))))
    (is (false? (predicate '(define [1]))))
    (is (false? (predicate '(define (1)))))
    (is (false? (predicate '(define '(1)))))
    (is (false? (predicate '(define '[1]))))
    (is (false? (predicate `(define #{'a 1}))))
    (is (false? (predicate '(define))))
    (is (false? (predicate '(-define-))))
    (is (false? (predicate '(['a]))))
    (is (false? (predicate '("define"))))
    (is (false? (predicate nil)))
    (is (false? (predicate '(define false))))
    (is (false? (predicate '(define "false"))))))
(deftest test-transform
  (testing "test"
    (is (= (transform '(define a nil))
           '[a nil]))
    (is (= (transform '(define a))
           '[a nil]))
    (is (= (transform '(define a 1))
           '[a 1]))))

(deftest test-reshape
  (testing "test my little lovely reshape-recur function"
    (is (=
         (reshape-recur '((define a 1) (print a) (define a 2) (print a)))
         '(nil (let [a 1]
            (print a)
            nil
            (let [a 2]
              (print a))))))
    (is (=
          (reshape-recur
           '(list
             (define a 2)
             a))
          '(list
            nil
            (let [a 2]
                   a))))))
(deftest test-macro
  (testing "test our macro lexical-do"
    (is (=
         (lexical-do
          (define a 1)
          (define b (+ a 1))
          (+ a b))
         3)))
  (testing "test closure"
    (let [counter
          (reshape-recur
           '(fn []
             (define c 0)
             (fn []
               (define c (+ 1 c))
               c)))]
      (is (=
           counter
           '(fn []
              nil
              (let [c 0]
                (fn []
                  nil
                  (let [c (+ 1 c)]
                    c)))))))) )
            

