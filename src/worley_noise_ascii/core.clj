(ns worley-noise-ascii.core
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(tufte/add-basic-println-handler! {})


(defn coords [w h]
  (for [y (range h) x (range w)]
    [x y]))

(defn random-sites [w h n]
  (take n (shuffle (coords w h))))

(defn manhattan_distance [x1 y1 x2 y2]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn m_dists [x1 y1 pts]
  "returns a sorted sequence of manhattan distances between (x1 y1) and pts"
  (p ::sorting (sort (p ::mapping-m-d (map #(manhattan_distance x1 y1 (first %1) (second %1)) pts)))))

(defn distance-vec [w h n]
  "NOTE: unused (faster version is dvec)
  Prodcues a list of vectors [x y d1, d2, d3...] where the ds are the
  distances to the closest, 2nd closest, &c feature point"
  (let [cs              (coords w h)
        feature_points  (take n (shuffle cs))]
    (vec
      (for [[x y] cs]
        (vec (concat [x y] (m_dists x y feature_points)))))))

(defn dvec [w h n]
  (let [feature_points (take n (repeatedly #(vector (rand-int w)
                                                    (rand-int h))))]
    (for [[x y] (p ::coords-in-dvec
                 (coords w h))]
      (p ::vec-and-concat (vec (concat [x y] (p ::m_dists (m_dists x y feature_points))))))))

;(time (prn (distance-vec 100 100 10))) ; 3.8 s
(prn (profile {} (def x (doall (dvec 70 100 10))))) ; 3.7 s ; fooled by laziness


(defn distance-block [d maxdist]
  (let [intervals (map #(* (/ maxdist 5) (inc %)) (range 5))
        blocks [\space \u2591 \u2592 \u2593 \u2588]
        dmap (into {} (map vector intervals blocks))]
    (some #(when (<= d %) (dmap %)) intervals)))

(defn distance-map [maxdist]
  (let [intervals (map #(* (/ maxdist 5) (inc %)) (range 5))
        blocks [\space \u2591 \u2592 \u2593 \u2588]]
    (into {} (map vector intervals blocks))))

(defn map-block [d dmap]
  (some #(when (<= d %) (dmap %)) (keys dmap)))

(defn print-noise [w h n rank]
  (let [d (map #(nth %1 (+ rank 2)) (dvec w h n))
        maxd (apply max d)
        dmap (distance-map maxd)
        blocks (map #(distance-block % maxd) d) ; this is slow b/c generating the map for every coordinate?
        maxline h]
    (loop [line 0
           bs blocks]
      (when (< line maxline)
        (println (apply str (take w bs)))  
        (recur (inc line) (drop w bs))))))

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


;(time (print-noise 80 24 20 0)) ; 1.308
;(time (print-noise-2 80 24 20 0)) ; 1.323 :hmm

(defn -main []
  (print-noise 80 24 20 0)
  (println)
  (print-noise 80 24 20 1)
  (println)
  (print-noise 80 24 20 2))

