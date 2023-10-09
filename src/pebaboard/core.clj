(ns pebaboard.core
  (:require [scad-clj.scad :as scad]
            [scad-clj.model :as m]
            [pebaboard.math :refer [cos sin sqrt atan2]])
  (:gen-class))

(defn deg2rad [deg]
  (/ (* Math/PI deg) 180))

(defn pow [base exponent]
  (reduce *' (repeat exponent base)))

(defn bezier-3 [P0 P1 P2]
  (fn [t]
    (+
      (* (pow (- 1 t) 2) P0)
      (* 2 (- 1 t) t P1)
      (* (pow t 2) P2))))

(defn b-tan [P0 P1 P2]
  (fn [t]
    (+ (* -2 (- t 1) P0)
       (* 2 (- (* 2 t) 1) P1)
       (* 2 t P2))))

(defn switch-hole [show-switch]
  (m/union
   (m/translate [0 0 -1]
                (m/difference
                 (m/cube 18 17 2)
                 (m/translate [0 0 -1.3]
                              (m/union
                               (m/translate [0 3.5 0] (m/cube 16 3.5 2))
                               (m/translate [0 -3.5 0] (m/cube 16 3.5 2))))
                 (m/translate [0 0 -1.3] (m/cube 14 14 5))))
   ;; (m/cube 17 18 0.3)
   (when show-switch
     (m/union
      (m/translate [0 0 3.9] (m/import "resources/r3single.stl"))
      (m/rotate [0 0 (deg2rad 180)] (m/import "resources/kailh_switch.stl"))))))

;; (spit "pebaboard.scad" (scad/write-scad (switch-hole true)))

(defn draw-line [[_ x1 y1] [_ x2 y2]]
  (m/hull
   (m/translate [0 x1 y1] (m/sphere 0.1))
   (m/translate [0 x2 y2] (m/sphere 0.1))))

(defn point-distance [[x1 y1] [x2 y2]]
  (sqrt (+ (pow (- x2 x1) 2)
           (pow (- y2 y1) 2))))

(defn curve-length [curve]
  (loop [acc 0 points curve]
    (if (nil? points)
      acc
      (let [current-point (first points)
            next-point (last (take 2 points))]
        (recur (+ acc (point-distance current-point next-point)) (next points))))))

(defn uniform-points [curve distance]
  (loop [acc [] points curve dist 0]
    (let [current-point (first points)
          next-point (last (take 2 points))]
      (if (nil? points)
        acc
        (if (> dist distance)
          (recur (conj acc next-point) (next points) 0)
          (recur acc (next points) (+ dist (point-distance current-point next-point))))))))

(defn p-ang [[x1 y1] [x2 y2]]
  (atan2 (- y2 y1)
         (- x2 x1)))

(defn draw-curve [[x1 y1] [x2 y2] [x3 y3]]
  (let [t (range 0 1.0 0.001)
        b3x (bezier-3 x1 x2 x3)
        xs (map b3x t)
        b3y (bezier-3 y1 y2 y3)
        ys (map b3y t)
        t3x (b-tan x1 x2 x3)
        tx (map t3x t)
        t3y (b-tan y1 y2 y3)
        ty (map t3y t)]
    (for [[x y tanx tany] (map vector xs ys tx ty)]
      {:x x :y y :tanx tanx :tany tany})))

(defn switch-locations [curve]
  (let [curve-points (map (juxt :x :y) curve)
        switch-points (uniform-points curve-points (/ (curve-length curve-points) 4))]
    (remove empty?
            (for [point curve]
              (let [switch-point (first (filter #(= [(:x point) (:y point)] %) switch-points))]
                (when switch-point
                  {:x (first switch-point)
                   :y (last switch-point)
                   :tanx (:tanx point)
                   :tany (:tany point)}))))))

(defn middle [[x1 y1 z1] [x2 y2 z2]]
  [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2) (/ (+ z1 z2) 2)])

(defn add-vector [[x1 y1 z1] [x2 y2 z2] n]
  [(+ (+ x1 x2) n) (+ (+ y1 y2) 2) (/ (+ z1 z2) n)])

(defn inner-product [u v]
  (apply + (map * u v)))

(defn norm [u]
  (Math/sqrt (inner-product u u)))

(defn mag-vec [[x y z]]
  (sqrt (+ (* x x) (* y y) (* z z))))

(defn len-vec [[x y z]]
  (sqrt (+ (pow x 2)
           (pow y 2)
           (pow z 2))))

(def ann
  (let [curve (draw-curve [0 50] [0 20] [50 0])
        switch (switch-locations curve)]
    (m/union
     (for [[x y tanx tany] (map (juxt :x :y :tanx :tany) switch)]
       (let [a (- (/ Math/PI 2) (atan2 tany tanx) )]
         (println tanx tany)
         (m/union
          ;; (draw-line [0 x y] [0 (+ (* 20 (cos a)) x) (+ (* 20 (sin a)) y)])
          (m/translate [0 x y] (m/rotate [(- (p-ang [x y] [(+ (* 20 (cos a)) x) (+ (* 20 (sin a)) y)]) (/ Math/PI 2)) 0 0] (switch-hole true))))))
     (for [[x y] (map (juxt :x :y) curve)]
       (m/union
        ;; (m/color [1 0 0] (m/translate [0 x y] (m/cube 0.1 0.1 0.1)))
        )))))

(spit "pebaboard.scad" (scad/write-scad
                        (m/union
                         (for [i (range 5)]
                           (case i
                             0 (m/translate [(* i 18) 0 0] ann)
                             1 (m/translate [(* i 18) -2 0] ann)
                             2 (m/translate [(* i 18) -7 0] ann)
                             3 (m/translate [(* i 18) 1 0] ann)
                             4 (m/translate [(* i 18) 10 0] ann))))))


;; (defn parse-line [line]
;;   (let [cu (-> line
;;                (str/replace "," "")
;;                (str/split #" "))
;;         [op & rest] cu
;;         args (map edn/read-string rest)]
;;     {(keyword (.toLowerCase op)) (into [] args)}))

;; (defn penis [beg end]
;;   (m/hull
;;    (m/translate beg (m/cube 0.01 0.01 0.01))
;;    (m/translate end (m/cube 0.01 0.01 0.01))))



;; (def key-points
;;   (for [line lines]
;;     (let [[x1 y1 z1 x2 y2 z2] line]
;;       {:point (middle [x1 y1 z1] [x2 y2 z2])})))

;; (defn -main
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))



;; (defn angulo [u v]
;;   (Math/acos (/ (inner-product u v)
;;                 (norm u)
;;                 (norm v))))

;; (defn ang-vec [vec]
;;   (map (partial angulo vec)
;;    '[[1 0 0]
;;      [0 1 0]
;;      [0 0 1]]))

;; (defn vec-rotate [v1]
;;   (m/rotate (ang-vec v1) (m/cube 0.5 0.5 0.5)))

;; (defn diff-vec [v1 v2]
;;   (map - v2 v1))

;; (let [p1 [0 0 0]
;;       p2 [0 5 5]]

;;   (ang-vec (diff-vec p2 p1))
;;   ;; (spit "pebaboard.scad" (scad/write-scad
;;   ;;                         (m/union
;;   ;;                          (penis p1 p2)
;;   ;;                          (m/translate (middle p1 p2)
;;   ;;                                       (m/rotate [(deg2rad 45) 0 0]
;;   ;;                                        (m/cube 0.5 0.5 0.5))))))
;;   )



;; (def lines
;;   (->> (slurp "lines")

;;        (str/split-lines)
;;        (map parse-line)
;;        (map :line)))

;; (defn put-cube [p]
;;   (m/translate p (m/cube 0.1 0.1 0.1)))

;; lines



;; (parse-line "LINE 3.12118, 2.76265, 1.90754, 3.11843, 2.80132, 1.71134, 8, 1, 1 ")
