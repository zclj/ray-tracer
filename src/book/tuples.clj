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

(s/def ::color (s/keys :req-un [::r ::g ::b ::w]))

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

(defn make-color
  [^double red ^double green ^double blue]
  {:r red :g green :b blue :w 0.0})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

(defn add-t
  [[a1 a2 a3 a4] [b1 b2 b3 b4]]
  [(+ a1 b1) (+ a2 b2) (+ a2 b2) (+ a2 b2)])


(defn add-m
  [t1 t2 [k1 k2 k3 k4]]
  {k1 (+ (k1 t1) (k1 t2))
   k2 (+ (k2 t1) (k2 t2))
   k3 (+ (k3 t1) (k3 t2))
   k4 (+ (k4 t1) (k4 t2))})

;; (s/fdef add
;;   :args (s/cat :tuple-1 ::tuple :tuple-2 ::tuple)
;;   :ret ::tuple)
;; (defn add
;;   [t1 t2]
;;   {:x (+ (:x t1) (:x t2))
;;    :y (+ (:y t1) (:y t2))
;;    :z (+ (:z t1) (:z t2))
;;    :w (+ (:w t1) (:w t2))})

(s/fdef add
  :args (s/or :tuple (s/cat :tuple-1 ::tuple :tuple-2 ::tuple)
              :color (s/cat :color-1 ::color :color-2 ::color))
  :ret (s/or :tuple ::tuple
             :color ::color))
(defn add
  [t1 t2]
  (if (s/valid? ::tuple t1)
    (add-m t1 t2 [:x :y :z :w])
    (add-m t1 t2 [:r :g :b :w])))

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
  :ret double?)
(defn dot
  [a b]
  (reduce
   + [(* (:x a) (:x b)) (* (:y a) (:y b)) (* (:z a) (:z b)) (* (:w a) (:w b))]))

(s/fdef cross
  :args (s/cat :vector-1 ::vector :vector-2 ::vector)
  :ret ::vector)
(defn cross
  [a b]
  (make-vector
   (- (* (:y a) (:z b)) (* (:z a) (:y b)))
   (- (* (:z a) (:x b)) (* (:x a) (:z b)))
   (- (* (:x a) (:y b)) (* (:y a) (:x b)))))
