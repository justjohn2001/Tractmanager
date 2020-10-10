(ns tractmanager.core)

(let
    [check (fn [& sets]
             (first (filter #(not (nil? %))
                            (map
                             (fn [ss]
                               (let [r (first (filter #(or (= % #{:x}) (= % #{:o})) ss))]
                                 (if r (first r) nil)))
                             sets))))]
  (defn ttt [board]
    (check
     (map set board)
     (map set (apply map list board))
     (list (set (map #(nth (nth board %) %) (range 3))))
     (list (set (map #(nth (nth board %) (- 2 %)) (range 3)))))))

(assert (= :x (ttt [[:x :o :x] [:x :o :o] [:x :x :o]])))
(assert (= :o (ttt [[:o :x :x] [:x :o :x] [:x :o :o]])))
(assert (nil? (ttt [[:x :o :x] [:x :o :x] [:o :x :o]])))

(comment 
  "1. The code checks tick-tack-toe boards for a winner.
2. `ttt` converts each of the \"lines\" that may contain a winning group on the board to a set and calls `check` on them. `check` determines if there is a set that only contains a single element which indicates a line that only holds that element, i.e. is a winning line.
3.
Line 2 - Use `defn-` instead of a local fn. Consider making `sets` be a list of sets rather than a list of lists of sets by concatenating them on the call. This would let you avoid avoid the `map` and one of the `filter`s. Finally, make the name more specific to what it is doing, such as `find-winner`
Line 3 - Use `some?` instead of `#(not (nil? %))` .
Line 6 - Use `#{#{:x} #{:o}}` as the predicate. Also, you can use `some` instead of `(first (filter ...))`.
Line 7 - Combine the `if` with the `let` above into `when-let`.
Line 10 - If you take the line 2 suggestion on changing the parameter, apply the `concat` here.
Line 13 - Use `get-in` rather than nested `nth`s.")

;;; 4.
;;; I like this. It is much clearer about what is being sought. However I would
;;; seek a code review to ensure it's not too concise.
(defn ttt-winner [board]
  (first (some #{[:x :x :x] [:o :o :o]}
               (concat
                board                                   ;; rows
                (apply map vector board)                ;; columns
                [(mapv #(get-in board [% %]) (range 3)) ;; diagonals
                 (mapv #(get-in board [% (- 2 %)]) (range 3))]))))
