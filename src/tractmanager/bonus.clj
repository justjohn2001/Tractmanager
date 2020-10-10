(ns tractmanager.bonus)

(comment "There are n=a+b+c+3 total edges.
To form a triangle, pick any 2 edges to be sides. If both come from the same vertex, then the third one must be from one of the other vertices. Otherwise the third may be chosen at random.
So the formula is:
n(n-1)(n-2)/6 - ((a+2)(a+1)a/6 + (b+2)(b+2)b/6 + (c+2)(c+1)c/6)")

(defn choose3 [n]
  (/ (* n (- n 1) (- n 2)) 6))

(defn number-of-triangles
  [a b c]
  (let [n (+ a b c 3)]
    (- (choose3 n) (choose3 (+ a 2)) (choose3 (+ b 2)) (choose3 (+ c 2)))))
