(import spork/charts)
(import spork/gfx2d)

(def cmap (get charts/color-maps :turbo))
(defn distfrom [px py] (fn [x y] (let [dx (- px x) dy (- py y)] (math/sqrt (+ (* dx dx) (* dy dy))))))
(def d1 (distfrom 10 10))
(def d2 (distfrom 20 18))
(def d3 (distfrom 37 8))

(charts/heat-map-chart
  :width (* 2 1920) :height (* 2 1080)
  :num-columns (* 1 48) :num-rows (* 1 27)
  :box-gap 2
  :font (gfx2d/load-font "examples/fonts/Roboto-Regular.ttf" 24)
  :title "Heat Map Example"
  :cell-text-fn (fn [x y] (string/format "%d,%d" x y))
  :color-fn (fn [x y]
              (def t (min (* 0.1 (d1 x y)) (* 0.2 (d2 x y)) (* 0.03 (d3 x y))))
              (cmap (+ (* 0.01 (math/random)) t)))
  :save-as "tmp/heat-map.png")
(print "Made heat map at tmp/heat-map.png")
