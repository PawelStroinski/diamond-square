(ns diamond-square.core
  (:require
    [goog.dom :as gdom]
    [reagent.core :as reagent]
    [diamond-square.terrain-interpolator :as ti])
  (:require-macros
    [diamond-square.core :as c]))

(defonce state
  (reagent/atom
    {:size-index 2
     :rand-amp 0.1
     :offset 0
     :magnitude-exp 0.1}))

(def points-vec (c/points-vec))

(def size #(first (nth points-vec (:size-index @state))))

(def points #(second (nth points-vec (:size-index @state))))

(defn matrix->pixels
  [m]
  (let [len (.-length m)
        max-val (.reduce m (fn [acc x] (max acc x)))
        multiplier (/ 255 max-val)
        ret (js/Uint8ClampedArray. (* len 4))]
    (.forEach m
      (fn [x i]
        (let [c (* x multiplier)
              r (- 255 c)
              j (* i 4)]
          (aset ret j r)
          (aset ret (+ j 1) r)
          (aset ret (+ j 2) c)
          (aset ret (+ j 3) c))))
    ret))

(defn draw
  [this]
  (let [ctx (gdom/getCanvasContext2D (reagent/dom-node this))
        m (ti/matrix (size))
        _ (ti/interpolate m
            (assoc (select-keys @state [:rand-amp :offset])
              :size (size)
              :points (points)
              :magnitude #(Math/pow % (:magnitude-exp @state))))
        pixels (matrix->pixels m)
        data (js/ImageData. pixels (size))]
    (.putImageData ctx data 0 0)))

(defn canvas
  []
  [^{:component-did-mount draw, :component-did-update draw}
   (fn [] [:canvas {:width (size), :height (size)}])])

(defn slider
  ([lbl k from to step] (slider lbl k from to step #(k @state)))
  ([lbl k from to step display]
   [:div
    [:input {:type "range"
             :id (name k)
             :min from
             :max to
             :step step
             :value (k @state)
             :on-change (fn [])
             :on-input (fn [event]
                         (swap! state assoc k
                           (js/parseFloat (-> event .-target .-value))))}]
    [:label {:for (name k)} (str lbl " (" (display) ")")]]))

(defn settings
  []
  [:div
   [slider "Size" :size-index 0 (dec (count points-vec)) 1 size]
   [slider "Random Amplitude" :rand-amp 0.1 10 0.1]
   [slider "Offset" :offset 0 10 0.1]
   [slider "Magnitude Exponent" :magnitude-exp 0 10 0.1]])

(defn app
  []
  [:div
   [settings]
   [canvas]])

(reagent/render-component [app] (gdom/getElement "app"))
