(ns book.canvas
  (:require [clojure.spec.alpha :as s]))

;; The canvas contains of rows of pixels.
;; Each pixel is represented by a vector of [red green blue]
;; The color value range from 0 - 255. Values lower or higher should be capped

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

(s/fdef make-pixel
  :args (s/cat :r ::pixel-color :g ::pixel-color :b ::pixel-color)
  :ret ::pixel)
(defn make-pixel
  [r g b]
  ;;[r g b]
  [0 0 0]
  )

(defn make-canvas-row
  [width]
  (vec (take width (repeat (make-pixel 0 0 0)))))

(s/fdef make-canvas
  :args (s/cat :width nat-int? :height nat-int?)
  :ret ::canvas)
(defn make-canvas
  [width height]
  (if (or (= 0 width) (= 0 height))
    [[]]
    (vec (take height (repeat (make-canvas-row width))))))

(defn width
  [canvas]
  (count (first canvas)))

(defn height
  [canvas]
  (count canvas))

(comment
  (apply = (reduce (fn [acc v]
                     (conj acc (count v))) [] [[[1 2 3]] [[1 2 3]]]))

  (same-size-rows? [[[1 2 3]] [[1 2 3] [1 2 3]]]))
