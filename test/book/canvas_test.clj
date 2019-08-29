(ns book.canvas-test
  (:require  [clojure.test :refer [deftest testing is are] :as t]
             [clojure.spec.alpha :as s]
             [clojure.spec.gen.alpha :as gen]
             [clojure.spec.test.alpha :as st]
             [orchestra.spec.test :as or]
             [expound.alpha :as expound]
             [book.canvas :as sut]))

(or/instrument)

(set! s/*explain-out* expound/printer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pixels
(deftest pixels-color-values-specification-should-range-from-0-to-255
  (is (not (s/valid? ::sut/pixel-color -1)))
  (is (not (s/valid? ::sut/pixel-color 256)))
  (is (s/valid? ::sut/pixel-color 0))
  (is (s/valid? ::sut/pixel-color 255))
  (is (s/valid? ::sut/pixel-color 123)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas

(deftest canvas-specification-should-have-rows-of-same-size
  (is (= false
         (s/valid? ::sut/canvas [[[1 2 3]] [[1 2 3] [1 2 3]]])))
  (is (= true
         (s/valid? ::sut/canvas [[[1 2 3]]])))
  (is (= false
         (s/valid? ::sut/canvas [])))
  (is (= true
         (s/valid? ::sut/canvas [[]]))))

;; Scenario: Creating a canvas
;; A new canvas should be initialized with black pixels
(deftest should-create-canvas
  (let [canvas (sut/make-canvas 10 20)]
    (is (= 10 (sut/width canvas)))
    (is (= 20 (sut/height canvas))))
  )

;;(sut/make-pixel 1 255 3)




(comment
  (gen/sample (s/gen ::sut/pixel))
  (gen/sample (s/gen ::sut/pixel-color) 100)

  (gen/sample (s/gen ::sut/pixel-row) 1)
  (gen/sample (s/gen ::sut/canvas) 1)

  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties

(defn test-fn
  [fn-name]
  (let [check-result (st/abbrev-result (first (st/check fn-name)))]
    (if (:failure check-result)
      (expound/explain-results (st/check fn-name))
      true)))

(deftest check-make-pixel
  (is (test-fn `sut/make-pixel)))

(deftest check-make-canvas
  (is (test-fn `sut/make-canvas)))

(or/unstrument)
;; Pixels color values 0-255
