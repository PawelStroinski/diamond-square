(ns diamond-square.terrain-interpolator)

(defn matrix
  [size]
  #?(:clj  (vec (repeat size (vec (repeat size 0))))
     :cljs (js/Float64Array. (* size size))))

(defn m-get
  [m w x y]
  #?(:clj  (get-in m [x y])
     :cljs (aget m (+ (* w y) x))))

(defn m-set
  [m w x y v]
  #?(:clj  (assoc-in m [x y] v)
     :cljs (do
             (aset m (+ (* w y) x) v)
             m)))

(defn average
  [m w points amp ofs]
  (-> (reduce (fn [r [x y]] (+ r (m-get m w x y))) 0 points)
    (/ (count points))
    (+ (rand amp))
    (+ ofs)))

(defn square
  [m w [x y] size amp ofs]
  (let [c #(int (+ % (/ size 2)))
        e #(+ % (dec size))
        cx (c x) cy (c y) ex (e x) ey (e y)]
    (m-set m w cx cy (average m w [[x y] [ex y] [x ey] [ex ey]] amp ofs))))

(defn diamond
  [m w [x y] size amp ofs]
  (let [c #(int (+ % (/ size 2)))
        b #(int (- % (/ (dec size) 2)))
        a #(int (+ % (dec size) (/ size 2)))
        e #(+ % (dec size))
        cx (c x) cy (c y) ex (e x) ey (e y) bx (b x) by (b y) ax (a x) ay (a y)
        avg #(average m w % amp ofs)
        v1 (avg (cond-> [[x y] [ex y] [cx cy]] (>= by 0) (conj [cx by])))
        v2 (avg (cond-> [[cx cy] [x y] [x ey]] (>= bx 0) (conj [bx cy])))
        v3 (avg (cond-> [[x ey] [ex ey] [cx cy]] (< ay w) (conj [cx ay])))
        v4 (avg (cond-> [[cx cy] [ex y] [ex ey]] (< ax w) (conj [ax cy])))]
    (-> m
      (m-set w cx y v1)
      (m-set w x cy v2)
      (m-set w cx ey v3)
      (m-set w ex cy v4))))

(defn is-power-of-two [x] (and (>= x 2) (zero? (bit-and x (dec x)))))

(defn half-size [size] (inc (/ (dec size) 2)))

(defn fourths
  [acc [x y] size]
  (if (> size 3)
    (let [c #(int (+ % (/ size 2)))
          cx (c x) cy (c y)
          h (half-size size)
          xys [[x y] [x cy] [cx y] [cx cy]]]
      (reduce (fn [acc [x y]] (fourths acc [x y] h))
        (update acc h (fnil into []) xys) xys))
    acc))

(defn interpolate-points
  [size]
  (fourths {size [[0 0]]} [0 0] size))

(defn interpolate
  ([m] (interpolate m {:size (count m)}))
  ([m {:keys [size rand-amp offset magnitude points]
       :or {rand-amp 0, offset 0, magnitude identity}}]
   (if (= size 1)
     m
     (let [_ (assert (and size (is-power-of-two (dec size))))
           pts (or points (interpolate-points size))
           sizes (sort-by - (keys pts))
           pass (fn [acc sz amp ofs f]
                  (reduce (fn [ret xy] (f ret size xy sz amp ofs))
                    acc (get pts sz)))]
       (reduce (fn [acc sz]
                 (let [mag (magnitude (/ (dec sz) (dec size)))
                       amp (* rand-amp mag)
                       ofs (* offset mag)]
                   (-> acc (pass sz amp ofs square) (pass sz amp ofs diamond))))
         m sizes)))))
