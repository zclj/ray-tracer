(ns book.tuples
  (:require [clojure.spec.alpha :as s]))

(s/def ::x float?)
(s/def ::y float?)
(s/def ::z float?)
(s/def ::w float?)

;; (s/def ::tuple (s/or :vec (s/cat :x float? :y float? :z float? :w float?)
;;                      :map (s/keys :req-un [::x ::y ::z ::w])))

(s/def ::tuple-cat (s/cat :x float? :y float? :z float? :w float?))
(s/def ::tuple (s/keys :req-un [::x ::y ::z ::w]))

(s/fdef point?
  :args (s/cat :tuple ::tuple)
  :ret boolean?
  :fn (s/or
       :point #(and (= (get-in % [:args :tuple :w]) 1.0) (:ret %))
       :no-point #(and (not= (get-in % [:args :tuple :w]) 1.0) (not (:ret %)))))

(s/exercise-fn `point?)

(defn point?
  [tuple]
  (= (:w tuple) 1.0))

(defn vector?
  [tuple]
  (= (:w tuple) 0.0))

(defn make-tuple
  [x y z w]
  (s/conform ::tuple-cat [x y z w]))
