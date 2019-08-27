(ns book.canvas
  (:require [clojure.spec.alpha :as s]))

;; The canvas contains of rows of pixels.
;; Each pixel is represented by a vector of [red green blue]
;; The color value range from 0 - 255. Values lower or higher should be capped

[[[1 2 3] [1 2 3]]]

(defn same-size-rows?
  [rows]
  (apply = (reduce (fn [acc v] (conj acc (count v))) [] rows)))

(s/def ::pixel-color (s/and int? #(>= % 0) #(<= % 255)))
(s/def ::pixel (s/coll-of ::pixel-color :count 3 :kind vector?))
(s/def ::pixel-row (s/coll-of ::pixel :kind vector?))
(s/def ::canvas (s/and (s/coll-of ::pixel-row)
                       #(if (> (count %) 0)
                          (same-size-rows? %)
                          false)))

(comment
  (apply = (reduce (fn [acc v]
                     (conj acc (count v))) [] [[[1 2 3]] [[1 2 3]]]))

  (same-size-rows? [[[1 2 3]] [[1 2 3] [1 2 3]]]))

(s/fdef make-pixel
  :args (s/cat :r ::pixel-color :g ::pixel-color :b ::pixel-color)
  :ret ::pixel)
(defn make-pixel
  [r g b]
  ;;[r g b]
  [0]
  )

(defn make-canvas
  [width height]
  {:canvas/width  width
   :canvas/height height})

(defn width
  [canvas]
  0)
