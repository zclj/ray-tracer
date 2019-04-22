(ns book.tuples-test
  (:require  [clojure.test :refer [deftest testing is are] :as t]
             [clojure.spec.alpha :as s]
             [clojure.spec.gen.alpha :as gen]
             [clojure.spec.test.alpha :as st]
             [orchestra.spec.test :as or]
             [expound.alpha :as expound]
             [book.tuples :as sut]))

;; Book Cp. 1 - Tuples.features

(or/instrument)

(set! s/*explain-out* expound/printer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example tests

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

(deftest should-be-able-to-create-a-point
  (is (= {:x 4.0 :y -4.0 :z 3.0 :w 1.0}
         (sut/make-point 4 -4 3))))

(deftest should-be-able-to-create-a-vector
  (is (= {:x 4.0 :y -4.0 :z 3.0 :w 0.0}
         (sut/make-vector 4 -4 3))))

;;;;
;; Operations on tuples
(deftest should-be-able-to-add-two-tuple
  (let [a1 (sut/make-tuple 3 -2 5 1)
        a2 (sut/make-tuple -2 3 1 0)]
   (is (= (sut/make-tuple 1 1 6 1)
          (sut/add a1 a2)))))

;; Scenario: Subtracting two points
;; Scenario: Subtracting a vector from a point
;; Scenario: Subtracting two vectors
;; Scenario: Subtracting a vector from the zero vector


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property tests

(defn test-fn
  [fn-name]
  (let [check-result (st/abbrev-result (first (st/check fn-name)))]
    (if (:failure check-result)
      (expound/explain-results (st/check `sut/vector?))
      true)))

(deftest test-point?
  (is (test-fn `sut/point?)))

(deftest test-vector?
  (is (test-fn `sut/vector?)))

(or/unstrument)

(comment
  (s/exercise ::sut/tuple)
  (gen/sample (s/gen ::sut/tuple))

  (s/exercise-fn `sut/point?)
  (stest/check `sut/point?))

