(ns diamond-square.core
  (:require [diamond-square.terrain-interpolator :as ti]))

(defmacro points-vec
  []
  (->> [7 8 9 10]
    (map #(inc (Math/pow 2 %)))
    (map (fn [size] [size (ti/interpolate-points size)]))
    (into [])))
