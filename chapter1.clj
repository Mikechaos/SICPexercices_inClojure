
(ns SICP)

(def else true)

(defn square [x] (* x x))
(defn average [x y] (/ (+ x y) 2))
(defn abs [x] (if (neg? x) (- x) x))

(def sin (fn [x] (Math/sin x))) ; encapsulate sin an cos
(def cos (fn [x] (Math/cos x))) ; so they can be use as first order functions


(defn gcd [a b]
  (loop [b b r (mod a b)]
    (if (zero? r) b
        (recur r (mod b r)))))


(defn sum
  "Summation from a to b of f(x) where x is the term and f is the next fn"
  [term a next b]
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))

(defn cube [x] (* x x x))

(defn sum-cubes [a b] (sum cube a inc b))


(defn integral [f a b dx]
  "Approximate (as dx gets smaller) in the integral of f from a to b"
  (defn add-dx [x] ( + x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; (defn integral-simpson [f a b n]
;;   (let [h (/ (- b a))]
;;     (* (sum f (+ a (* 
;;     ))

;; (defn sum-it [term a next b]
;;   (loop [

nil ; 1.3.3

(defn search [f neg pos]
  (let [mid (average neg pos)
        close-enough? (fn [x y] (< (abs (- x y)) 0.001))]
    (if (close-enough? neg pos)
      mid
      (let [test-value (f mid)]
        (cond (pos? test-value)
              (search f neg mid)
              (neg? test-value)
              (search f mid pos)
              (zero? test-value)
              mid)))))

(defn half-interval-method [f a b]
  (let [a-val (f a)
        b-val (f b)]
    (cond (and (neg? a-val) (pos? b-val))
          (search f a b)
          (and (neg? b-val) (pos? a-val))
          (search f b a)
          else
          "Error, a and b not of opposite sign")))

(defn fixed-point [f first-guess]
  (def tolerance 0.00001)
  (defn close-enough? [v1 v2] (< (abs (- v1 v2)) tolerance))
  (loop [guess first-guess]
    (let [next (f guess)]
    (if (close-enough? guess next)
      next
      (recur next)))))

(def golden-ratio ;not working yet
  (fixed-point #(average % (+ % (/ 1 %))) 1.0))

(defn deriv [g]
  (def dx 0.0001)
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx))) 

(defn newton-tranform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))
(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn sqrt [x] ; nope...
  (newtons-method (fn [y] (- (square y) x))
                  1.0))

(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn sqrt [x]
  (fixed-point-of-transform #(/ x %)
                            average-damp
                            1.0))
