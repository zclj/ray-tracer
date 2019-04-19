(ns book.tuples-test
  (:require  [clojure.test :refer [deftest testing is are] :as t]
             [clojure.spec.alpha :as s]
             [clojure.spec.gen.alpha :as gen]
             [clojure.spec.test.alpha :as st]
             [orchestra.spec.test :as stest]
             [book.tuples :as sut]))

(stest/instrument)
;;(stest/instrument `sut/point?)
;; Book Cp. 1 - Tuples.features

(sut/point? {:x 1.0 :y 1.0 :z 1.0 :w 1.0})
(st/check `sut/point?)
;;(stest/unstrument `book.tuples)

(deftest a-tuple-with-w-1.0-should-be-a-point
  (let [a (sut/make-tuple 4.3 -4.2 3.1 1.0)]
    (are [x y] (= x y)
      4.3   (:x a)
      -4.2  (:y a)
      3.1   (:z a)
      1.0   (:w a))
    (is (= true (sut/point? a)))
    (is (= false  (sut/vector? a)))))

(deftest a-tuple-with-w-1.0-should-be-a-vector
  (let [a (sut/make-tuple 4.3 -4.2 3.1 0.0)]
    (are [x y] (= x y)
      4.3  (:x a)
      -4.2 (:y a)
      3.1  (:z a)
      0.0  (:w a))
    (is (= false (sut/point? a)))
    (is (= true  (sut/vector? a)))))

(comment
  (s/exercise ::sut/tuple)
  (gen/sample (s/gen ::sut/tuple))

  (s/exercise-fn `sut/point?)
  (stest/check `sut/point?))

