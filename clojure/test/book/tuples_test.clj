(ns book.tuples-test
  (:require  [clojure.test :refer [deftest testing is are] :as t]
             [clojure.spec.alpha :as s]
             [clojure.spec.gen.alpha :as gen]
             [clojure.spec.test.alpha :as st]
             [orchestra.spec.test :as or]
             [expound.alpha :as expound]
             [book.tuples :as sut]))

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
(deftest should-be-able-to-add-two-tuples
  (let [a1 (sut/make-tuple 3 -2 5 1)
        a2 (sut/make-tuple -2 3 1 0)]
   (is (= (sut/make-tuple 1 1 6 1)
          (sut/add a1 a2)))))

(deftest should-be-able-to-subtract-two-points
  (let [p1 (sut/make-point 3 2 1)
        p2 (sut/make-point 5 6 7)]
   (is (= (sut/make-vector -2 -4 -6)
          (sut/sub p1 p2)))))

;; Scenario: Subtracting a vector from a point
(deftest should-be-able-to-subtract-a-vector-from-a-point
  (let [p (sut/make-point 3 2 1)
        v (sut/make-vector 5 6 7)]
   (is (= (sut/make-point -2 -4 -6)
          (sut/sub p v)))))

;; Scenario: Subtracting two vectors
(deftest should-be-able-to-subtract-two-vectors
  (let [v1 (sut/make-vector 3 2 1)
        v2 (sut/make-vector 5 6 7)]
   (is (= (sut/make-vector -2 -4 -6)
          (sut/sub v1 v2)))))

;; Scenario: Subtracting a vector from the zero vector
(deftest should-be-able-to-subtract-a-vector-from-the-zero-vector
  (let [zero (sut/make-vector 0 0 0)
        v (sut/make-vector 1 -2 3)]
   (is (= (sut/make-vector -1 2 -3)
          (sut/sub zero v)))))

(deftest should-be-able-to-negate-a-tuple
  (let [a (sut/make-tuple 1 -2 3 -4)]
   (is (= (sut/make-tuple -1 2 -3 4)
          (sut/neg a)))))

;; Scenario: Multiplying a tuple by a scalar
(deftest should-be-able-to-multiply-a-tuple-by-a-scalar
  (let [a (sut/make-tuple 1 -2 3 -4)]
   (is (= (sut/make-tuple 3.5 -7 10.5 -14)
          (sut/mul a 3.5)))))

;; Scenario: Multiplying a tuple by a fraction
(deftest should-be-able-to-multiply-a-tuple-by-a-fraction
  (let [a (sut/make-tuple 1 -2 3 -4)]
   (is (= (sut/make-tuple 0.5 -1 1.5 -2)
          (sut/mul a 0.5)))))

;; Scenario: Dividing a tuple by a scalar
(deftest should-be-able-to-divide-a-tuple-by-a-scalar
  (let [a (sut/make-tuple 1 -2 3 -4)]
   (is (= (sut/make-tuple 0.5 -1 1.5 -2)
          (sut/div a 2)))))

;; Scenario: Computing the magnitude of vector(1, 0, 0)
;; Scenario: Computing the magnitude of vector(0, 1, 0)
;; Scenario: Computing the magnitude of vector(0, 0, 1)
;; Scenario: Computing the magnitude of vector(1, 2, 3)
;; Scenario: Computing the magnitude of vector(-1, -2, -3)
(deftest should-compute-magnitude-of-vectors
  (let [v (sut/make-vector 1 0 0)]
    (is (= 1.0 (sut/magnitude v)))
    (is (= 1.0 (sut/magnitude (sut/make-vector 0 1 0))))
    (is (= 1.0 (sut/magnitude (sut/make-vector 0 0 1))))
    (is (= (Math/sqrt 14) (sut/magnitude (sut/make-vector 1 2 3))))
    (is (= (Math/sqrt 14) (sut/magnitude (sut/make-vector -1 -2 -3))))))

;; Scenario: Normalizing vector(4, 0, 0) gives (1, 0, 0)
;; Scenario: Normalizing vector(1, 2, 3)
(deftest should-normalize-vectors
  (is (= (sut/make-vector 1 0 0) (sut/normalize (sut/make-vector 4 0 0))))
  (is (= (sut/make-vector
          (/ 1 (Math/sqrt 14)) (/ 2 (Math/sqrt 14)) (/ 3 (Math/sqrt 14)))
         (sut/normalize (sut/make-vector 1 2 3)))))

;; Scenario: The magnitude of a normalized vector
(deftest should-give-the-magnitude-of-a-normalized-vector
  (is (= 1.0 (sut/magnitude (sut/normalize (sut/make-vector 1 2 3))))))

;; Scenario: The dot product of two tuples
(deftest should-calculate-dot-product-of-two-vectors
  (is (= 20.0 (sut/dot (sut/make-vector 1 2 3) (sut/make-vector 2 3 4)))))

;; Scenario: The cross product of two vectors
(deftest should-calculate-cross-product-of-two-vectors
  (is (= (sut/make-vector -1 2 -1)
         (sut/cross (sut/make-vector 1 2 3) (sut/make-vector 2 3 4))))
  (is (= (sut/make-vector 1 -2 1)
         (sut/cross (sut/make-vector 2 3 4) (sut/make-vector 1 2 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors

;; Scenario: Colors are (red, green, blue) tuples
(deftest should-be-able-to-create-a-color
  (is (= {:r 4.0 :g -4.0 :b 3.0 :w 0.0}
         (sut/make-color 4 -4 3))))

;; Scenario: Adding colors
(deftest should-be-able-to-add-colors
  (is (= (sut/make-color 1.6 0.7 1.0)
         (sut/add (sut/make-color 0.9 0.6 0.75)
                  (sut/make-color 0.7 0.1 0.25)))))

;; Scenario: Subtracting colors
(deftest should-be-able-to-subtract-colors
  (is (= true
         (sut/eq (sut/make-color 0.2 0.5 0.5)
                 (sut/sub (sut/make-color 0.9 0.6 0.75)
                          (sut/make-color 0.7 0.1 0.25))
                 0.00001))))

;; Scenario: Multiplying a color by a scalar
(deftest should-be-able-to-multiply-color-with-scalar
  (is (= (sut/make-color 0.4 0.6 0.8)
         (sut/mul (sut/make-color 0.2 0.3 0.4)
                  2))))

;; Scenario: Multiplying colors
(deftest should-be-able-to-multiply-colors
  (is (= true
         (sut/eq
          (sut/make-color 0.9 0.2 0.04)
          (sut/mul-t (sut/make-color 1 0.2 0.4)
                     (sut/make-color 0.9 1 0.1))
          0.00001))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property tests

(defn test-fn
  [fn-name]
  (let [check-result (st/abbrev-result (first (st/check fn-name)))]
    (if (:failure check-result)
      (expound/explain-results (st/check fn-name))
      true)))

(deftest test-point?
  (is (test-fn `sut/point?)))

(deftest test-vector?
  (is (test-fn `sut/vector?)))


(comment
  (st/check `sut/op-m {:gen {:op #(gen/return +)}})
  
  (st/check `sut/add-m {:gen {::sut/add-m-args
                              #(gen/return [{:a 1 :b 2 :c 3 :d 4}
                                            {:a 1 :b 2 :c 3 :d 4}
                                            [:a :b :c :d]])}})

  (gen/sample (gen/set (gen/keyword) {:num-elements 4}))

  (gen/sample (gen/tuple (gen/double) (gen/double)
                         (gen/double) (gen/double)))
  
  (gen/sample (s/gen ::sut/add-m-args
                     {::sut/add-m-args
                      #(gen/return [{:a 1 :b 2 :c 3 :d 4}
                                    {:a 1 :b 2 :c 3 :d 4}
                                    [:a :b :c :d]])}))

  (gen/sample (gen/return [{:a 1 :b 2 :c 3 :d 4}
                           {:a 1 :b 2 :c 3 :d 4}
                           [:a :b :c :d]]))

  (or/instrument))

(or/unstrument)

(comment
  (s/exercise ::sut/tuple)
  (gen/sample (s/gen ::sut/tuple))

  (s/exercise-fn `sut/point?)
  (stest/check `sut/point?))

