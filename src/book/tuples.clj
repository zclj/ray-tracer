(ns book.tuples
  (:refer-clojure :exclude [vector?])
  (:require [clojure.spec.alpha :as s]))

(s/def ::x float?)
(s/def ::y float?)
(s/def ::z float?)
(s/def ::w float?)

;; (s/def ::tuple (s/or :vec (s/cat :x float? :y float? :z float? :w float?)
;;                      :map (s/keys :req-un [::x ::y ::z ::w])))

(s/def ::tuple-cat (s/cat :x float? :y float? :z float? :w float?))
(s/def ::tuple (s/keys :req-un [::x ::y ::z ::w]))

(s/def ::vector (s/and (s/keys :req-un [::x ::y ::z ::w])
                       #(= (get % :w) 0.0)))

(s/fdef point?
  :args (s/cat :tuple ::tuple)
  :ret boolean?
  :fn (s/or
       :point #(and (= (get-in % [:args :tuple :w]) 1.0) (:ret %))
       :no-point #(and (not= (get-in % [:args :tuple :w]) 1.0) (not (:ret %)))))

(defn point?
  [tuple]
  (= (:w tuple) 1.0))

(s/fdef vector?
  :args (s/cat :tuple ::tuple)
  :ret boolean?
  :fn (s/or
       :point #(and (= (get-in % [:args :tuple :w]) 0.0) (:ret %))
       :no-point #(and (not= (get-in % [:args :tuple :w]) 0.0) (not (:ret %)))))

(defn vector?
  [tuple]
  (= (:w tuple) 0.0))

(defn make-tuple
  [^double x ^double y ^double z ^double w]
  (s/conform ::tuple-cat [x y z w]))

(defn make-point
  [^double x ^double y ^double z]
  (make-tuple x y z 1.0))

(defn make-vector
  [^double x ^double y ^double z]
  (make-tuple x y z 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

(s/fdef add
  :args (s/cat :tuple-1 ::tuple :tuple-2 ::tuple)
  :ret ::tuple)
(defn add
  [t1 t2]
  {:x (+ (:x t1) (:x t2))
   :y (+ (:y t1) (:y t2))
   :z (+ (:z t1) (:z t2))
   :w (+ (:w t1) (:w t2))})

(s/fdef sub
  :args (s/cat :tuple-1 ::tuple :tuple-2 ::tuple)
  :ret ::tuple)
(defn sub
  [t1 t2]
  {:x (- (:x t1) (:x t2))
   :y (- (:y t1) (:y t2))
   :z (- (:z t1) (:z t2))
   :w (- (:w t1) (:w t2))})

(s/fdef neg
  :args (s/cat :tuple ::tuple)
  :ret ::tuple)
(defn neg
  [t]
  {:x (- (:x t))
   :y (- (:y t))
   :z (- (:z t))
   :w (- (:w t))})

(s/fdef mul
  :args (s/cat :tuple ::tuple :scalar double?)
  :ret ::tuple)
(defn mul
  [t s]
  {:x (* (:x t) s)
   :y (* (:y t) s)
   :z (* (:z t) s)
   :w (* (:w t) s)})

(s/fdef div
  :args (s/cat :tuple ::tuple :scalar double?)
  :ret ::tuple)
(defn div
  [t ^double s]
  {:x (/ (:x t) s)
   :y (/ (:y t) s)
   :z (/ (:z t) s)
   :w (/ (:w t) s)})

(s/fdef magnitude
  :args (s/cat :vector ::vector)
  :ret double?)
(defn magnitude
  [v]
  (Math/sqrt (reduce + (map #(Math/pow (get v %) 2) [:x :y :z :w]))))

(s/fdef normalize
  :args (s/cat :vector ::vector)
  :ret ::vector)
(defn normalize
  [v]
  {:x (/ (:x v) (magnitude v))
   :y (/ (:y v) (magnitude v))
   :z (/ (:z v) (magnitude v))
   :w (/ (:w v) (magnitude v))})

(s/fdef dot
  :args (s/cat :vector-1 ::vector :vector-2 ::vector)
  :ret ::vector)
(defn cross
  [a b]
  (make-vector
   (- (* (:y a) (:z b)) (* (:z a) (:y b)))
   (- (* (:z a) (:x b)) (* (:x a) (:z b)))
   (- (* (:x a) (:y b)) (* (:y a) (:x b)))))
