(ns nonograms-dojo.core)

(defn transpose [m]
  (apply mapv vector m))

(def input-dot-clj
  {:size [10 10]
   :rows [[] [1 1] [1] [2 1 1] [1 1 1] [1 2 1 1] [1] [1] []    []]
   :cols [[] [1]   []  [3]     [1 1]   []        [5] [1] [1 4] []]})

(def input-2
  {:size [5 5]
   :rows [[2 2] [2] [1] [1 1] [1 3]]
   :cols [[2 2] [2] [1] [1 1] [1 3]] })

(def output-2
  [[:X :X :_ :X :X]
   [:X :X :_ :_ :_]
   [:_ :_ :_ :_ :X]
   [:X :_ :_ :_ :X]
   [:X :_ :X :X :X]])

(defn initial-solution [size]
  (repeat (first size) (into [] (repeat (second size) :?))))

;; filling functions

(defn fill-line-completely [hint]
  (into [] (flatten (interpose :_ (mapv (fn [number] (repeat number :X)) hint)))))

(defn fill-from-start
  [line number]
  (concat (repeat number :X) [:_] (take-last (- (count line) (+ number 1)) line)))

(defn fill-from-end [line number]
  (concat (take (- (count line) (+ number 1)) line) [:_] (repeat number :X)))


(defn line-complete? [line]
  (contains? line :?))

(defn replace-in-line [line search replacement]
  (mapv (fn [element] (if (= element search)
                        replacement
                        element))
        line))

(defn count-X [line]
  (count (filter #(= :X %)
                 line)))

(defn starts-with-X? [line]

  (= :X (first line)))

(defn ends-with-X? [line]
  (= :X (last line)))

;; solve sum

(defn line-solve-sum [line hint]
  (let [sum-hint (reduce + hint)
        min-spaces (- (count hint) 1)]
    (if (= (+ sum-hint min-spaces)
           (count line))
      (fill-line-completely hint)
      line)))

(defn lines-solve-sum [matrix hints]
  (into [] (map-indexed (fn [i line] (line-solve-sum line (hints i)))
                        matrix)))

(defn matrix-solve-sum [matrix rows cols]
  (-> (lines-solve-sum matrix rows)
      transpose
      (lines-solve-sum cols)
      transpose))

;;; solve sides

(defn try-fill-from-start [line number]
  (if (starts-with-X? line)
    (fill-from-start line number)
    line))

(defn try-fill-from-end [line number]
  (if (ends-with-X? line)
    (fill-from-end line number)
    line))

(defn line-solve-sides [line hint]
  (if (line-complete? line)
    line
    (-> line
        (try-fill-from-start (first hint))
        (try-fill-from-end (last hint)))))

(defn lines-solve-sides [matrix hints]
  (into [] (map-indexed (fn [i line] (line-solve-sides line (hints i)))
                        matrix)))

(defn matrix-solve-sides [matrix rows cols]
  (-> matrix
      (lines-solve-sides rows)
      transpose
      (lines-solve-sides cols)
      transpose))

;;; solve spaces

(defn line-solve-spaces [line hint]
  (if (line-complete? line)
    line
    (if (= (count-X line)
           (reduce + hint))
      (replace-in-line line :? :_)
      line)))

(defn lines-solve-spaces [matrix hints]
  (into [] (map-indexed (fn [i line] (line-solve-spaces line (hints i)))
                        matrix)))

(defn matrix-solve-spaces [matrix rows cols]
  (-> matrix
      (lines-solve-spaces rows)
      transpose
      (lines-solve-spaces cols)
      transpose))

;; putting everything together

(defn solve
  [{:keys [size rows cols] :as input}]
  (let [solution (initial-solution size)]
    (-> solution
        (matrix-solve-sum rows cols)
        (matrix-solve-sides rows cols)
        (matrix-solve-spaces rows cols))))
