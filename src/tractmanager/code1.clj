(ns tractmanager.code1)

(def screen {1 [0 0]
             2 [0 1]
             3 [0 2]
             4 [1 0]
             5 [1 1]
             6 [1 2]
             7 [2 0]
             8 [2 1]
             9 [2 2]})

(def connections [[-1 -1] [-1 0] [-1 1]
                  [0 -1]         [0 1]
                  [1 -1]  [1 0]  [1 1]
                  [-2 -1] [-2 1]
                  [-1 -2] [1 -2]
                  [-1 2]  [1 2]
                  [2 -1]  [2 1]])

(defn- on-pad?
  [x y [dx dy]]
  (and (<= 0 (+ x dx) 2)
       (<= 0 (+ y dy) 2)))

(defn- valid-moves
  [[x y] used]
  (let [used-positions (into #{} (map screen) used)
        base-moves (sequence (comp (filter (partial on-pad? x y))
                                   (map (fn [[dx dy]]
                                          [(+ x dx) (+ y dy)])))
                             connections)]
    (reduce (fn [moves [px py :as p]]
              (let [[dx2 dy2 :as double-p] [(* 2 (- px x)) (* 2 (- py y))]]
                (if (used-positions p)
                  (if (on-pad? x y double-p)
                    (conj moves [(+ x dx2) (+ y dy2)])
                    moves)
                  (conj moves p))))
            []
            base-moves
            )))

(defn valid-path [pin]
  (let [[start & more-pin] pin
        start-location (screen start)]
    (if (nil? start-location)
      false
      (loop [used #{start}
             [digit & pin'] more-pin
             location (screen start)]
        (let [next-locations (set (valid-moves location used))]
          (cond
            (nil? digit) true
            (used digit) false
            (contains? next-locations
                       (screen digit)) (recur (conj used digit)
                                              pin'
                                              (screen digit))
            :else false))))))

;;; Bonus question 1
(defn count-paths [path]
  (if (valid-path path)
    (inc (apply + (map #(count-paths (conj path %)) (range 1 10))))
    0))

(def total-patterns (+ (* 4 (count-paths [1]))
                       (* 4 (count-paths [2]))
                       (count-paths [5]))) ; Due to symmetry, the number of patterns that start on 1, 3, 7, and 9 are the same as are the number of patterns that start on 2, 4, 6, and 8.
;;; 389497
(def numeric-pin-size (first (drop-while #(< (apply * (repeat % 9)) total-patterns) (range))))
;;; 6 - So a 6 digit PIN would have more combinations than the pattern lock.

(/ (apply * (map #(- 81 %) (range 8))) (Math/pow 62 8) )

;;; Bonus question 2
(comment "I'm going to estimate this rather than modifying the code to find it.

There are 62^8 8 character alphanumeric passwords given that character set.
For a grid of size n, with only the 'no repeats' rule there would be
(n^2)!/((n^2)-8)! 8 dot patterns with no repeats. With the rules, there should
be fewer (by a factor of ~ 1.36 when n=3). For n=8, there are fewer patterns
than 8 character alphanumeric passwords. For n=9, there are almost 6
times as many patterns with 'no-repeats'. My expectation is that the rules
would eliminate fewer than 1/ln(n^2) since the eliminations are based
on the number of reducible fractions generated in the set of i/j where
(i, j in [1..n]). So I would expect less than 1/4 of the patterns to be
eliminated leaving more than 4 times the number of patterns a 8
character passwords.
Therefor, I expect that a 9x9 grid would have more 8 dot patterns than the
number of 8 character alphanumeric passwords.")
