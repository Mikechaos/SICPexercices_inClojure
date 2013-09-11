(defn make-rat [n d]
  (let [g (gcd n d)
        n (/ n g)
       d (/ d g)]
  (cons n (cons d '()))))

(def numer first)
(def denom last)

(defn generic-add-rat [op x y] ; used a generic function with op as +
                               ; or -
(make-rat
   (op (* (denom y) (numer x)) ; add the numerators
      (* (denom x) (numer y)))
   (* (denom y) (denom x)))) ; and return the right denom
  

(def add-rat (partial generic-add-rat +))
(def sub-rat (partial generic-add-rat -))

(defn mult-rat [x y]
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))


(defn div-rat [x y]
  (make-rat
   (* (numer x) (denom y))
   (* (numer y) (denom x))))

(defn equal-rat [x y]
  (=
   (* (numer x) (denom y))
   (* (numer y) (denom x))))

(defn print-rat [x]
  (println)
  (print (numer x))
  (print "/")
  (print (denom x)))


; Exercise 2.2

(defn generic-make-pairs [x y]
  (cons x (cons y '())))

(def make-point generic-make-pairs)

(def x-point first)
(def y-point last)

(def make-segment generic-make-pairs)

(def start-segment first)
(def end-segment last)

(defn midpoint-segment [seg]
  (let [[[x1 y1] [x2 y2]] seg] ; A little destructuring to get each coord
    (make-point (average x1 x2)
                (average y1 y2))))

(defn segment-length [seg]
  (let [[[x1 y1] [x2 y2]] seg]
    (Math/sqrt (+ (square (- x2 x1))

; Exercise 2.3
; first implementation                  
(defn make-rectangle [tlc brc] ;top left corner and bottom right
  (let [[x1 y1] tlc
        [x2 y2] brc
        trc (make-point x2 y1) ; top right
        blc (make-point x1 y2)] ; bottom left
    [(make-segment tlc trc) (make-segment trc brc)
     (make-segment brc blc) (make-segment blc tlc)]))
                  (square (- y2 y1))))))

(def get-base-seg first)
(def get-side-seg last)

; second implementation
(def make-rectangle generic-make-pairs)

(defn get-base-seg [rec]
  (let [[[x1 y1] [x2 y2]] rec]
    (make-segment (make-point x1 y1) (make-point x1 y2))))

(defn get-side-seg [rec]
  (let [[[x1 y1] [x2 y2]] rec]
    (make-segment (make-point x1 y1) (make-point x2 y1))))

; methods
(defn perimeter-rec [rec]
  (+ (* 2 (segment-length (get-base-seg rec)))
     (* 2 (segment-length (get-side-seg rec)))))

(defn area-rec [rec]
  (* (segment-length (get-base-seg rec))
     (segment-length (get-side-seg rec))))

(defmacro define-selectors [name-first-selector name-last-selector] ; not quite..
  (do
  `(def ~name-first-selector first)
  `(def ~name-last-selector last)))

; Exercise 2.4
(defn my-cons [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

; Exercise 2.5
(defn logn [n x] (/ (Math/log x) (Math/log n)))
(def log2 (partial logn 2))
(def log3 (partial logn 3))

(defn my-cons [a b]
  (* (Math/pow 2 a)
     (Math/pow 3 b)))

(defn car [c]
  (
