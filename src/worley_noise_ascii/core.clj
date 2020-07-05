(ns worley-noise-ascii.core)

(defn coords [w h]
  (for [y (range h) x (range w)]
    [x y]))

(defn random-sites [w h n]
  (take n (shuffle (coords w h))))

(defn manhattan_distance [x1 y1 x2 y2]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn m_dists [x1 y1 pts]
  "returns a sorted sequence of manhattan distances between (x1 y1) and pts"
  (sort (map #(manhattan_distance x1 y1 (first %1) (second %1)) pts)))

(defn dvec [w h n]
  "returns a collection of vectors [x y d1 d2...]
   where x y are the coordinates, and di is the ith closest feature point.
   The n feature points are chosen uniformly at random from the w*h space"
  (let [feature_points (take n (repeatedly #(vector (rand-int w)
                                                    (rand-int h))))]
    (for [[x y] (coords w h)]
      (vec (concat [x y] (m_dists x y feature_points))))))

(defn distance-block [d maxdist]
  "replaces distance d with unicode block characters of different values
   evenly spaced between 0 and maxdist"
  (let [intervals (map #(* (/ maxdist 5) (inc %)) (range 5))
        blocks [\space \u2591 \u2592 \u2593 \u2588]
        dmap (into {} (map vector intervals blocks))]
    (some #(when (<= d %) (dmap %)) intervals)))

(defn distance-block-syms [d maxdist syms]
  "replaces numerical distance with unicode character given by the associated
   symbol in syms"
  (let [num_chars (count syms)
        intervals (map #(* (/ maxdist num_chars) (inc %)) (range num_chars))
        dmap (into {} (map vector intervals syms))]
    (some #(when (<= d %) (dmap %)) intervals)))

(def emojis ["😀" "😇" "😈" "😎" "🙃" "😳"])

(defn distance-map [maxdist]
  (let [intervals (map #(* (/ maxdist 5) (inc %)) (range 5))
        blocks [\space \u2591 \u2592 \u2593 \u2588]]
    (into {} (map vector intervals blocks))))

(defn map-block [d dmap]
  (some #(when (<= d %) (dmap %)) (keys dmap)))

(defn print-noise [w h n rank]
  "slow because of all the sorting in generating the dvec"
  (let [d (map #(nth %1 (+ rank 2)) (dvec w h n))
        maxd (apply max d)
        blocks (map #(distance-block % maxd) d)
        maxline h]
    (loop [line 0
           bs blocks]
      (when (< line maxline)
        (println (apply str (take w bs)))  
        (recur (inc line) (drop w bs))))))

(defn print-noise-syms [w h n rank syms]
  "uses a custom vector of unicode symbols to displace noise"
  (let [d (map #(nth %1 (+ rank 2)) (dvec w h n))
        maxd (apply max d)
        dmap (distance-map maxd)
        blocks (map #(distance-block-syms % maxd syms) d)
        maxline h]
    (loop [line 0
           bs blocks]
      (when (< line maxline)
        (println (apply str (take w bs)))  
        (recur (inc line) (drop w bs))))))

;(print-noise-syms 13 10 5 2 emojis)

(defn print-noise-2 [w h n rank]
  (let [d (map #(nth %1 (+ rank 2)) (dvec w h n))
        maxd (apply max d)
        dmap (distance-map maxd)
        blocks (map #(map-block % dmap) d)
        maxline h]
    (loop [line 0
           bs blocks]
      (when (< line maxline)
        (println (apply str (take w bs)))  
        (recur (inc line) (drop w bs))))))

(defn -main []
  ;(print-noise 80 24 20 0)
  ;(println)
  (print-noise 80 24 20 1)
  (println)
  (print-noise 80 24 20 2))

