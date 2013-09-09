(ns SICP)
(defn
  "Summation from a to b of f(x) where x is the term and f is the next fn"
  sum [term a next b]
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))

(defn cube [x] (* x x x))

(defn sum-cubes [a b] (sum cube a inc b))


(defn integral [f a b dx]
  (defn add-dx [x] ( + x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
