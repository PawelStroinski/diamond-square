(ns diamond-square.terrain-interpolator-test
  (:require [midje.sweet :refer :all]
            [diamond-square.terrain-interpolator :refer :all]))

(def m3 (matrix 3))
(def m5 (matrix 5))

(defmacro with-log
  [& body]
  `(let [~'log (atom [])
         log-conj# (fn [s#]
                     #(swap! ~'log conj
                        (cons s# (->> %& (drop 2) (drop-last 2)))))]
     (with-redefs [square (log-conj# '~'square)
                   diamond (log-conj# '~'diamond)]
       ~@body)))

(defn matrix-roughly [m] (just (map #(just (map roughly %)) m)))

(facts "simple validations"
  (fact "terminal condition - size one"
    (interpolate (matrix 1)) => (matrix-roughly (matrix 1)))

  (fact "size must be power of two plus 1"
    (interpolate (matrix 2)) => (throws AssertionError))

  (tabular "is power of two"
    (fact (is-power-of-two ?x) => ?exp)
    ?x ?exp
    2 true
    4 true
    8 true

    1 false
    7 false
    18 false))

(facts "square diamond coordinate calculations"
  (facts "simple 3x3"
    (fact "overview"
      (with-log
        (interpolate m3)
        @log => '((square [0 0] 3) (diamond [0 0] 3))))

    (fact "square pass"
      (square m3 3 [0 0] 3 0 0) => (assoc-in m3 [1 1] 'a)
      (provided
        (average m3 3 [[0 0] [2 0] [0 2] [2 2]] 0 0) => 'a))

    (fact "diamond pass"
      (diamond m3 3 [0 0] 3 0 0) => (-> m3
                                      (assoc-in [1 0] 'a) (assoc-in [0 1] 'b)
                                      (assoc-in [1 2] 'c) (assoc-in [2 1] 'd))
      (provided
        (average m3 3 [[0 0] [2 0] [1 1]] 0 0) => 'a
        (average m3 3 [[1 1] [0 0] [0 2]] 0 0) => 'b
        (average m3 3 [[0 2] [2 2] [1 1]] 0 0) => 'c
        (average m3 3 [[1 1] [2 0] [2 2]] 0 0) => 'd)))

  (facts "5x5 first pass"
    (fact "overview"
      (with-log
        (interpolate m5)
        (take 2 @log) => '((square [0 0] 5) (diamond [0 0] 5))))

    (fact "square pass"
      (square m5 5 [0 0] 5 0 0) => (assoc-in m5 [2 2] 'a)
      (provided
        (average m5 5 [[0 0] [4 0] [0 4] [4 4]] 0 0) => 'a))

    (fact "diamond pass"
      (diamond m5 5 [0 0] 5 0 0) => (-> m5
                                      (assoc-in [2 0] 'a) (assoc-in [0 2] 'b)
                                      (assoc-in [2 4] 'c) (assoc-in [4 2] 'd))
      (provided
        (average m5 5 [[0 0] [4 0] [2 2]] 0 0) => 'a
        (average m5 5 [[2 2] [0 0] [0 4]] 0 0) => 'b
        (average m5 5 [[0 4] [4 4] [2 2]] 0 0) => 'c
        (average m5 5 [[2 2] [4 0] [4 4]] 0 0) => 'd)))

  (facts "5x5 second diamond pass"
    (fact
      (diamond m5 5 [2 2] 3 0 0) => (-> m5
                                      (assoc-in [3 2] 'a) (assoc-in [2 3] 'b)
                                      (assoc-in [3 4] 'c) (assoc-in [4 3] 'd))
      (provided
        (average m5 5 [[2 2] [4 2] [3 3] [3 1]] 0 0) => 'a
        (average m5 5 [[3 3] [2 2] [2 4] [1 3]] 0 0) => 'b
        (average m5 5 [[2 4] [4 4] [3 3]] 0 0) => 'c
        (average m5 5 [[3 3] [4 2] [4 4]] 0 0) => 'd))

    (fact
      (diamond m5 5 [0 2] 3 0 0) => (-> m5
                                      (assoc-in [1 2] 'a) (assoc-in [0 3] 'b)
                                      (assoc-in [1 4] 'c) (assoc-in [2 3] 'd))
      (provided
        (average m5 5 [[0 2] [2 2] [1 3] [1 1]] 0 0) => 'a
        (average m5 5 [[1 3] [0 2] [0 4]] 0 0) => 'b
        (average m5 5 [[0 4] [2 4] [1 3]] 0 0) => 'c
        (average m5 5 [[1 3] [2 2] [2 4] [3 3]] 0 0) => 'd))))

(fact "square/diamond repetition - 5x5"
  (with-log
    (interpolate m5)
    @log =>
    '[(square [0 0] 5) (diamond [0 0] 5)
      (square [0 0] 3) (square [0 2] 3) (square [2 0] 3) (square [2 2] 3)
      (diamond [0 0] 3) (diamond [0 2] 3) (diamond [2 0] 3) (diamond [2 2] 3)]))

(facts "averages"
  (fact "zero" (interpolate m3) => (matrix-roughly m3))
  (fact "all ones" (interpolate [[1 0 1] [0 0 0] [1 0 1]])
    => (matrix-roughly [[1 1 1] [1 1 1] [1 1 1]]))
  (fact "ramp" (interpolate [[0 0 12] [0 0 0] [12 0 24]])
    => (matrix-roughly [[0 8 12] [8 12 16] [12 16 24]])))

(fact "randoms and offsets - volcano"
  (with-redefs [rand identity]
    (interpolate m5 {:size 5, :rand-amp 2, :offset 4}) =>
    (matrix-roughly
      [[0 8.5 8 8.5 0]
       [8.5 8.5 10.75 8.5 8.5]
       [8 10.75 6 10.75 8]
       [8.5 8.5 10.75 8.5 8.5]
       [0 8.5 8 8.5 0]])))
