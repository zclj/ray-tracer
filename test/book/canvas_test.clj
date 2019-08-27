(ns book.canvas-test
  (:require  [clojure.test :refer [deftest testing is are] :as t]
             [clojure.spec.alpha :as s]
             [clojure.spec.gen.alpha :as gen]
             [clojure.spec.test.alpha :as st]
             [orchestra.spec.test :as or]
             [expound.alpha :as expound]
             [book.canvas :as sut]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pixels
(deftest pixels-color-values-should-range-from-0-to-255
  (is (not (s/valid? ::sut/pixel-color -1)))
  (is (not (s/valid? ::sut/pixel-color 256)))
  (is (s/valid? ::sut/pixel-color 0))
  (is (s/valid? ::sut/pixel-color 255))
  (is (s/valid? ::sut/pixel-color 123)))


;; Scenario: Creating a canvas
;; A new canvas should be initialized with black pixels
(deftest should-create-canvas
  (let [canvas (sut/make-canvas 10 20)]
   (is (= 10 (sut/width canvas))))
  )

(comment
  (gen/sample (s/gen ::sut/pixel))
  (gen/sample (s/gen ::sut/pixel-color) 100)

  (gen/sample (s/gen ::sut/pixel-row) 1)
  (gen/sample (s/gen ::sut/canvas) 1)

  (s/valid? ::sut/canvas [[[1 2 3]] [[1 2 3] [1 2 3]] [[1 2 3] [1 2 3]]])
  (s/valid? ::sut/canvas [[[1 2 3]]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties

;; Pixels color values 0-255
