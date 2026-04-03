###
### charts.janet
###
### NOTE: Beta-quality - apis may change.
###
### This module is for generating graphs and charts on the CPU and rendering
### them to bitmaps. While not completely general in styling, charts should be general
### purpose in visualizing different kinds of data. For more rendering backends or functionality,
### libraries like plPlot may be more suitable. However, out-of-the-box chart generation with minimal
### dependencies is very useful to have.
###
### Data is passed to most charts as a "data-frame", which is a table mapping string column names
### to arrays of data points.
###
### Data frame example:
###
### {:timestamp [1 2 3 4 5 6]
###  :temperature-1 [75.1 75.2 75.4 75.5 75.5 75.4]
###  :temperature-2 [55.1 55.4 55.7 60.0 60.4 60.9]}
###

### TODO
### [x] - horizontal legend should still be able to wrap vertically if too wide.
### [x] - LABEL YOUR AXES!
### [x] - wrap colors, padding, font, etc. into some kind of styling table to pass around consistently
### [x] - stippled grid lines
### [x] - bar chart
### [x] - area chart
### [ ] - horizontal bar charts
### [ ] - multi-bar charts
### [ ] - flame graph
### [ ] - packing chart
### [x] - heat map
### [ ] - error bars on line chart
### [ ] - fill between chart
### [x] - handle nils in y-columns for sparse data
### [x] - easier custom chart annotations in the metric space (horizontal lines, vertical lines, etc.)
### [ ] - captions and sub-titles

(import spork/gfx2d :as g)

# Defaults
(defdyn *font* "Default font for chart rendering")
(defdyn *text-color* "Default font color for title and axis labels")
(defdyn *stroke-color* "Default color for drawn lines such as frame borders")
(defdyn *background-color* "Default background color for chart rendering")
(defdyn *grid-color* "Default color for grid lines")
(defdyn *padding* "Default padding for charts")
(defdyn *color-seed* "Random seed to use when picking pseudo-random colors for charts")

# Default defaults
(def- default-font :olive)
(def- default-text-color g/black)
(def- default-stroke-color g/black)
(def- default-background-color g/white)
(def- default-grid-color (g/rgb 0.8 0.8 0.8))
(def- default-padding 16)
(def- default-width 1280)
(def- default-height 720)

(defn- check-enum-impl
  "Assert that a value x is in options, and give a nice error if not"
  [arg-name x options optstring]
  (assert (get options x) (string/format "expected argument %v to be one of %s, got %v" arg-name optstring x)))

(defmacro- enum
  [x & options]
  "Shorthand to assert that a value x is in options, and give a nice error if not"
  (def quote-options (invert options))
  (def optstring (string/join (map describe options) ", "))
  ~(,check-enum-impl ',x ,x ',quote-options ,optstring))

(defn- draw-frame
  "Draw a frame enclosing a rectangle that is `outer` pixels wide"
  [image x1 y1 x2 y2 color &opt outer]
  (default outer 1)
  (g/plot-path image [x1 y1 x1 y2 x2 y2 x2 y1] color 0 0 true)
  (if (> outer 1)
    (draw-frame image (dec x1) (dec y1) (inc x2) (inc y2) color (dec outer))
    image))

(defn- text-measure
  "Measure text either using simple text or a TTF font"
  [text &opt font scale orientation]
  (default font :default)
  (default scale 1)
  (default orientation 0)
  (if (abstract? font)
    (g/measure-text font text scale orientation)
    (g/measure-simple-text text font scale scale orientation)))

(defn- text-draw
  "Draw text either using simple text or a TTF font."
  [image x y text color &opt font scale orientation]
  (default font :default)
  (default scale 1)
  (default orientation 0)
  # Uncomment to check text bounding boxes for layout calculations
  #(def [w h] (text-measure text font scale orientation))
  #(draw-frame image x y (+ x w) (+ y h) color)
  (if (abstract? font)
    (g/draw-text image font x y text color scale orientation)
    (g/draw-simple-text image x y text color font scale scale orientation)))

(defn- floorn
  "Floor mod n"
  [n x]
  (def x :shadow (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (* (math/floor (/ x n)) n))

(defn- ceiln
  "Ceil mod n"
  [n x]
  (def x :shadow (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (* (math/ceil (/ x n)) n))

(defn- color-value
  "Gray scale value of a color"
  [c]
  (def [r g b a] (g/as-srgb c))
  (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))

(defn- lerp [x y t] (+ (* x t) (* y (- 1 t))))
(defn- clamp [x a b] (cond (< x a) a (< b x) b x))

(defn- color-hash
  "Given a value, generate a pseudo-random color for visualization"
  [x &opt color-seed]
  (default color-seed (dyn *color-seed*))
  (def rng (math/rng (hash [x color-seed])))
  (g/rgb (+ 0.2 (* 0.6 (math/rng-uniform rng)))
         (+ 0.2 (* 0.6 (math/rng-uniform rng)))
         (+ 0.2 (* 0.6 (math/rng-uniform rng)))))

###
### Graph Axes Calculation and rendering
###

(defn- calculate-data-bounds
  "Given a data frame, return [min-x max-x min-y max-y].
  Use this information for calculating render transform. Should handle non-existant columns."
  [data x-column y-columns
   &opt
   width height min-spacing
   override-min-x override-max-x
   override-min-y override-max-y]

  # Just skip all the guesswork
  (when (and override-min-x override-max-x override-min-y override-max-y)
    (break [override-min-x override-max-x override-min-y override-max-y]))

  # Calculate precise bounds for all x and y values
  (var min-y math/inf)
  (var max-y math/-inf)
  (var min-x (or (extreme < (filter identity (get data x-column))) 0))
  (var max-x (or (extreme > (filter identity (get data x-column))) 1))
  (each c y-columns
    (set min-y (min min-y (extreme < (filter identity (get data c [math/inf])))))
    (set max-y (max max-y (extreme > (filter identity (get data c [math/-inf]))))))

  # Now possibly expand bounds for nice axis ticks in the same way as `guess-axis-ticks`.
  # e.g. [1-99] -> [0-100]
  # Guess delta - making some assumptions is ok since we don't know exact measurements until axes layout and this is just to auto-fit.
  # Better to over-estimate deltas (metric tick spacing) than under-estimate here.
  # Full control is still available to the library user via x-min, x-max, y-min, and y-max.
  (def max-x-ticks (max 1 (math/floor (/ width min-spacing))))
  (def max-y-ticks (max 1 (math/floor (/ height min-spacing))))
  (def x-delta (/ (- max-x min-x) max-x-ticks))
  (def y-delta (/ (- max-y min-y) max-y-ticks))
  (def fudge-factor 1)

  # If minimums are a little over a nice number, set them to the nice number
  (def fudge-min-x (floorn x-delta min-x))
  (def fudge-min-y (floorn y-delta min-y))
  (if (< (- min-x fudge-min-x) (* fudge-factor x-delta))
    (set min-x fudge-min-x))
  (if (< (- min-y fudge-min-y) (* fudge-factor y-delta))
    (set min-y fudge-min-y))

  # If the maximums are a little under a nice number, set them to the nice number
  (def fudge-max-x (ceiln x-delta max-x))
  (def fudge-max-y (ceiln y-delta max-y))
  (if (< (- fudge-max-x max-x) (* fudge-factor x-delta))
    (set max-x fudge-max-x))
  (if (< (- fudge-max-y max-y) (* fudge-factor y-delta))
    (set max-y fudge-max-y))

  [(or override-min-x min-x) (or override-max-x max-x)
   (or override-min-y min-y) (or override-max-y max-y)])

(defn- guess-axis-ticks
  "Given a set of numeric values, generate a reasonable array of tick marks given a minimum spacing.
  Biases the tick spacing to a power of 10 (or by 5s) for nicer charts by default"
  [minimum maximum pixel-span min-spacing vertical font prefix suffix min-delta &opt force-formatter no-retry] # TODO - too many unnamed arguments
  (default suffix "")
  (default prefix "")
  (var max-ticks (math/floor (/ pixel-span min-spacing)))
  (if (zero? max-ticks) (break @[]))
  (def result (array/new max-ticks))
  (var delta (/ (- maximum minimum) max-ticks))
  (var metric minimum)

  # Bias delta towards a power of 10 for nice tick intervals
  # TODO - allow for other bases
  (def delta-log10 (math/log10 delta))
  (set delta (math/pow 10 (math/ceil delta-log10)))

  # Allow for steps of 5 as well
  (if (> (- (math/ceil delta-log10) delta-log10) (math/log10 2))
    (*= delta 0.5))
  # e.g. allow limiting to integers
  (when min-delta
    (set delta (max min-delta delta)))
  (def epsilon (* delta 0.001))

  # Get tick metrics
  (set metric (floorn delta metric))
  (while (< metric (- minimum epsilon))
    (+= metric delta))
  (while (<= metric (+ epsilon maximum))
    (array/push result metric)
    (+= metric delta))

  # Get a function that will format each tick mark for drawing based on their spacing
  (def formatter
    (or force-formatter
        (if (>= delta 1)
          (fn :formatter-int [x] (string/format "%s%d%s" prefix (math/round x) suffix))
          (let [fmt-string (string "%s%." (math/ceil (- (math/log10 delta))) "f%s")]
            (fn :formatter [x] (string/format fmt-string prefix x suffix))))))

  # Check maximum size of tick text
  (var max-text-width 0)
  (var max-text-height 0)
  (def padding10 10)
  (each metric-coord result
    (def [x y] (text-measure (formatter metric-coord) font 1))
    (set max-text-width (max max-text-width x))
    (set max-text-height (max max-text-height y)))

  # Recalculate
  (def min-spacing :shadow (+ padding10 (if vertical max-text-height max-text-width)))

  # Retry if ticks are too close together
  (unless no-retry
    (if (> (+ padding10 min-spacing) delta)
      (break (guess-axis-ticks minimum maximum pixel-span min-spacing vertical font prefix suffix min-delta force-formatter true))))

  # TODO - use text boundaries to set padding
  [result formatter max-text-width max-text-height])

###
### API
###

(defn dark-mode
  ```
  Set dynamic color defaults to dark mode
  ```
  []
  (setdyn *background-color* g/black)
  (setdyn *grid-color* (g/rgb 0.3 0.3 0.3))
  (setdyn *stroke-color* g/white)
  (setdyn *text-color* g/white))

(defn light-mode
  ```
  Set dynamic color defaults to light mode
  ```
  []
  (setdyn *background-color* g/white)
  (setdyn *grid-color* (g/rgb 0.8 0.8 0.8))
  (setdyn *stroke-color* g/black)
  (setdyn *text-color* g/black))

(defn color-lerp
  [a b t]
  "Linearly interpolate between 2 colors in RGB space. Colors are srgb encoded as 32 bit unsigned integers."
  (def [ar ag ab aa] (g/as-srgb a))
  (def [br bg bb ba] (g/as-srgb b))
  (g/srgb
    (lerp ar br t)
    (lerp ag bg t)
    (lerp ab bb t)
    (lerp aa ba t)))

(defn color-map
  "Create a function that linearly interpolates between colors for colormapping."
  [& colors]
  (def n-colors (length colors))
  (def n-1-colors (- n-colors 1))
  (fn :interp
    [t]
    (def t :shadow (clamp t 0 1))
    (def a-index (math/floor (* t n-1-colors)))
    (def b-index (+ 1 a-index))
    (if (> b-index n-1-colors) (break (last colors)))
    (def t-at-a (/ a-index n-1-colors))
    (def t-at-b (/ b-index n-1-colors))
    (def ab-interval (- t-at-b t-at-a))
    (def u (clamp (/ (- t t-at-a) ab-interval) 0 1))
    (color-lerp (in colors b-index) (in colors a-index) u)))

(defn invert-color-map
  "Create an inverted color-map from an existing color map."
  [mapping]
  (fn :inverted-map [t] (mapping (- 1 t))))

(def color-maps
  ```
  A table containing various default color maps that can be used for rendering heat map data.
  Each value is a function mapping real numbers in the range [0, 1] to colors represented as 32 bit integers.
  ```
  @{:grayscale (color-map g/black g/white)
    :bluescale (color-map 0xFF330000 g/white)
    :redscale (color-map 0xFF000033 g/white)
    :greenscale (color-map 0xFF003300 g/white)
    :bluescale-black (color-map g/black g/blue)
    :redscale-black (color-map g/black g/red)
    :greenscale-black (color-map g/black g/green)
    :turbo
    (color-map
      0xFF3D1331 0xFF742B39 0xFFA34140 0xFFCA5845 0xFFE56D47 0xFFF88246
      0xFFFF9641 0xFFF7AC34 0xFFE8BF26 0xFFD2D21A 0xFFBDE018 0xFFA9EC23
      0xFF90F53A 0xFF74FA58 0xFF5AFE78 0xFF43FE97 0xFF38FAAD 0xFF34F1C3
      0xFF35E6D6 0xFF39D7E8 0xFF3AC7F4 0xFF36B4FC 0xFF2F9FFE 0xFF2587FC
      0xFF1A6FF7 0xFF1157EE 0xFF0A44E3 0xFF0533D4 0xFF0325C4 0xFF0118AE
      0xFF010E97 0xFF03047B)
    :magma
    (color-map
      0xFF030000 0xFF0F0202 0xFF1F0709 0xFF310C11 0xFF41101A 0xFF551125
      0xFF671032 0xFF720F3E 0xFF79104B 0xFF7E1558 0xFF7F1963 0xFF811F71
      0xFF81247E 0xFF812889 0xFF802C95 0xFF7E30A3 0xFF7B34AE 0xFF7738BB
      0xFF723DC8 0xFF6D42D3 0xFF674ADE 0xFF6154E8 0xFF5D60F0 0xFF5B6EF6
      0xFF5D7DF9 0xFF628AFB 0xFF6999FD 0xFF73A8FE 0xFF7CB5FE 0xFF88C4FE
      0xFF95D2FD 0xFFA1DFFD)
    :viridis
    (color-map
      0xFF16000D 0xFF1D000F 0xFF24010F 0xFF2D030F 0xFF34050F 0xFF39090E
      0xFF3D0C0D 0xFF41100B 0xFF441609 0xFF451B08 0xFF462107 0xFF462705
      0xFF462D05 0xFF463504 0xFF463B03 0xFF454403 0xFF444D02 0xFF415602
      0xFF3D5F02 0xFF396903 0xFF347505 0xFF2E7F09 0xFF288A0F 0xFF219318
      0xFF199D26 0xFF12A837 0xFF0CAF4B 0xFF07B665 0xFF03BD88 0xFF01C3AB
      0xFF01C9D3 0xFF03CDFA)})

(defn draw-legend
  ```
  Draw a legend given a set of labels and colors

  `canvas` can be either nil to skip drawing or a gfx2d/Image.

  * :background-color - the color of the background of the legend
  * :font - the font to use for legend text
  * :padding - the number of pixels to leave around all drawn content
  * :color-map - a table/struct that maps labels to colors
  * :legend-map - a table/struct that maps labels to text to draw
  * :line-color - color to draw frame border
  * :text-color - color of text
  * :labels - a list of labels to draw in the legend
  * :view-width - width of the enclosing view in pixels to help hint how to size the legend.
  * :frame - whether or not to draw a frame around the legend

  Return [w h] of the area that was or would be drawn if the legend were to be drawn.
  ```
  [canvas &named
   background-color font padding color-map labels view-width
   frame color-seed legend-map line-color text-color]
  (default font (dyn *font* default-font))
  (default padding (dyn *padding* default-padding))
  (default color-map {})
  (default legend-map {})
  (default view-width 0)
  (default background-color (dyn *background-color* default-background-color))
  (default line-color (dyn *grid-color* default-grid-color))
  (default text-color (dyn *text-color* default-text-color))
  (when canvas
    (def {:width width :height height} (g/unpack canvas))
    (when frame (g/fill-rect canvas 0 0 width height background-color)))
  (def label-height (let [[_ h] (text-measure "Mg" font 1)] h))
  (def swatch-size label-height)
  (def spacing (+ label-height padding 1))
  (def small-spacing (math/round (* 0.125 label-height)))
  # (def padding (if frame (+ padding 4) padding)) # add frame border
  (var y padding)
  (var x padding)
  (var max-x 0)
  (each i labels
    (def lab (string (get legend-map i i)))
    (def [text-width _] (text-measure lab font 1))
    (def item-width (+ padding padding padding text-width swatch-size))
    (when (> (+ x item-width) view-width)
      (unless (= i (first labels)) (+= y spacing)) # don't skip first line
      (set x padding))
    (when canvas
      (def color (get color-map i (color-hash i)))
      (g/fill-rect canvas x y swatch-size swatch-size color)
      (text-draw canvas (+ x swatch-size padding) (+ small-spacing y) lab text-color font 1))
    (+= x (+ item-width padding))
    (set max-x (max max-x x)))
  (+= y (+ 1 padding))
  (when (and canvas frame)
    (def {:width width :height height} (g/unpack canvas))
    (draw-frame canvas 1 1 (- width 2) (- height 2) line-color 2)) # 2 pixel solid frame
  [max-x (+ label-height y)])

(defn- draw-color-map
  ```
  Draw a rectangle that describes a color-map. Will draw the gradient
  horizontally by default, but layout can be one of :h, :v, :horizontal, or :vertical.
  ```
  [canvas color-map x y w h &opt layout]
  (default layout :horizontal)
  (enum layout :horizontal :vertical :h :v)
  (def horiz (or (= layout :h) (= layout :horizontal)))
  (if horiz
    (for xx 0 w
      (def color (color-map (/ xx (- w 1))))
      (g/fill-rect canvas (+ x xx) y 1 h color))
    (for yy1 0 h
      (def yy (- h yy1 1)) # flip
      (def color (color-map (/ yy1 (- h 1))))
      (g/fill-rect canvas x (+ y yy) w 1 color)))
  canvas)

(defn draw-heat-legend
  ```
  Draw a legend that describes a heat-map color range.

  `canvas` can be either nil to skip drawing or a gfx2d/Image.

  * :swatch-width - width of the color gradient in pixels
  * :swatch-height - height of the color gradient in pixels
  * :background-color - the color of the background of the legend
  * :font - the font to use for legend text
  * :padding - the number of pixels to leave around all drawn content
  * :color-map - a table/struct that maps labels to colors
  * :line-color - color to draw frame border
  * :text-color - color of text
  * :labels - a list of labels to draw in the legend
  * :frame - whether or not to draw a frame around the legend

  Return [w h] of the area that was or would be drawn if the legend were to be drawn.
  ```
  [canvas &named
   swatch-width swatch-height
   background-color font padding color-map
   frame line-color text-color layout labels]
  (default font (dyn *font* default-font))
  (default padding (dyn *padding* default-padding))
  (default background-color (dyn *background-color* default-background-color))
  (default line-color (dyn *grid-color* default-grid-color))
  (default text-color (dyn *text-color* default-text-color))
  (default labels [])

  (default layout :horizontal)
  (enum layout :horizontal :vertical :v :h)
  (def h (or (= layout :h) (= layout :horizontal)))
  (def font-scale 1)

  # Measure extra padding needed by labels
  (var [lw lh] [0 0])
  (each l labels
    (def [tw th] (text-measure l font font-scale))
    (set lw (max lw tw))
    (set lh (max lh th)))
  (def h-padding (+ padding (div lw 2)))
  (def v-padding (+ padding (div lh 2)))

  # Default length should be enough to fit all of the labels along the long axis
  (def default-len (math/ceil (max 256 (* (length labels) (+ 4 (max lw lh))))))
  (default swatch-width (if h default-len 64))
  (default swatch-height (if h 64 default-len))

  (when canvas
    (def {:width width :height height} (g/unpack canvas))
    (when frame (g/fill-rect canvas 0 0 width height background-color)))

  (when canvas
    (draw-color-map canvas color-map (if h h-padding padding) (if h padding v-padding) swatch-width swatch-height layout))

  # Draw metric labels
  (when canvas
    (def llen (length labels))
    (for i 0 llen
      (def l (get labels i))
      (def t (/ i (dec llen)))
      (def [tw th] (text-measure l font font-scale))
      (if h
        (let [x (+ h-padding (math/floor (* t swatch-width)))]
          (text-draw canvas (- x (div tw 2)) (+ padding swatch-height padding) l text-color font font-scale 0))
        (let [y (+ v-padding (math/floor (* (- 1 t) swatch-height)))]
          (text-draw canvas (+ padding swatch-width padding) (- y (div th 2)) l text-color font font-scale 0)))))

  (when (and canvas frame)
    (def {:width width :height height} (g/unpack canvas))
    (draw-frame canvas 1 1 (- width 2) (- height 2) line-color 2)) # 2 pixel solid frame

  [(+ h-padding h-padding padding (if h (- padding) 0) swatch-width)
   (+ v-padding v-padding padding (if h 0 (- padding)) swatch-height)])

(defn draw-axes
  ```
  Draw the axis for the chart. Also return a function that can be used
  to convert a coordinate in the metric space to the screen space. Most parameters
  are optional with sane defaults, but canvas, x-min, x-max, y-min, y-max are all required.

  * :x-label - optional label for the x axis
  * :y-label - optional label for the y axis
  * :padding - the number of pixels to leave around all drawn content
  * :inner-padding - how many pixels to add between the axes frame and the internal graphing area. Defaults to 8.
  * :font - the font to use for axis text
  * :{x,y}-{min,max} - The bounds for coordinate system to draw
  * :grid - Style for drawing grid-lines. Can be nil (none), :none, :solid, or :stipple
  * :format-x - unary function (fn [x] ...) that returns a string to label x axis tick marks with
  * :format-y - unary function (fn [y] ...) that returns a string to label y axis tick marks with
  * :x-prefix - if format-x not provided, allows easily adding a string prefix to x axis tick mark labels
  * :y-prefix - if format-y not provided, allows easily adding a string prefix to y axis tick mark labels
  * :x-suffix - if format-x not provided, allows easily adding a string suffix to x axis tick mark labels
  * :y-suffix - if format-y not provided, allows easily adding a string suffix to y axis tick mark labels
  * :x-ticks - Allow setting specific tick marks to be used marking the x axis rather than making a guess.
  * :x-minor-ticks - How many minor tick marks, if any, to place between major tick marks on the x axis
  * :y-minor-ticks - How many minor tick marks, if any, to place between major tick marks on the y axis
  * :x-labels-vertical - Turn x labels vertical so more can fit on the axis
  * :min-x-spacing - When guessing x ticks, allow setting a lower limit to the metric spacing between ticks
  * :min-y-spacing - When guessing y ticks, allow setting a lower limit to the metric spacing between ticks
  * :tick-length - how many pixels long to make major tick marks (minor tick marks are 1/2 major tick marks)

  Returns a tuple [view:gfx2d/Image to-pixel-space:fn to-metric-space:fn]

  * `view` is an image that can be used to draw inside the chart, clipped so you don't overwrite that axes.
  * `(to-pixel-space metric-x metric-y)` converts metric space coordinates to pixel space for plotting on `view`.
  * `(to-metric-space pixel-x pixel-y)` converts pixel coordinates to the metric space.
  ```
  [canvas &named padding inner-padding font
   x-min x-max y-min y-max min-x-spacing min-y-spacing
   grid format-x format-y
   x-label y-label tick-length
   x-suffix x-prefix y-suffix y-prefix
   x-ticks x-minor-ticks y-minor-ticks x-labels-vertical]

  (default padding (dyn *padding* default-padding))
  (default font (dyn *font* default-font))
  (default grid :none)
  (assert canvas)
  (assert x-min)
  (assert x-max)
  (assert y-min)
  (assert y-max)

  # Check enums
  (enum grid :none :solid :stipple)

  (def {:width width :height height} (g/unpack canvas))
  (assert (pos? width))
  (assert (pos? height))
  (def line-color (dyn *stroke-color* default-stroke-color))
  (def grid-color (dyn *grid-color* default-grid-color))

  (def orig-dx (- x-max x-min))
  (def orig-dy (- y-max y-min))
  (assert (pos? orig-dx))
  (assert (pos? orig-dy))
  (def font-height (let [[_ h] (text-measure "Mg" font 1)] h))
  (default inner-padding 8)
  (def font-half-height (div font-height 2))
  (default tick-length 16)
  (def has-grid (not= grid :none))
  (def stipple-cycle (if (= grid :stipple) 8 0))
  (def stipple-on 4)
  (def tick-height (if has-grid 10 (+ tick-length 6)))
  (def tick-trim (if has-grid 0 (- tick-height tick-length)))

  # Initial guess for x label width
  (def [_xticks _xformat x-labels-width x-labels-height]
    (if x-ticks
      (do
        (def fmt (if format-x format-x string))
        (var maxh 0)
        (each xt x-ticks
          (def [w h] (text-measure (fmt xt) font 1))
          (set maxh (max maxh (if x-labels-vertical w h))))
        [nil nil maxh maxh])
      (guess-axis-ticks x-min x-max width 20 x-labels-vertical font x-prefix x-suffix min-x-spacing format-x)))

  # Calculate top and bottom padding
  (def outer-top-padding (max padding font-half-height))
  (def outer-bottom-padding (+ padding font-height (if x-label (+ padding (if x-labels-vertical x-labels-width x-labels-height)) 0)))
  (def top-padding outer-top-padding)
  (def bottom-padding (+ outer-bottom-padding tick-height))

  # Draw X Label
  (when x-label
    (def [w _h] (text-measure x-label font 1))
    (def yy (- height padding font-height))
    (text-draw canvas (div (- width w) 2) yy x-label line-color font 1))

  # Guess y axis ticks - used to calculate left and right padding
  (def [yticks yformat y-axis-tick-label-width]
    (guess-axis-ticks y-min y-max (- height top-padding bottom-padding) 20 true font y-prefix y-suffix min-y-spacing format-y))

  # Calculate left and right padding once y-axis is guessed
  (def outer-left-padding (+ padding y-axis-tick-label-width (if y-label (+ padding font-height) 0)))
  (def outer-right-padding outer-left-padding) # make it symmetrical, looks much nicer
  #(def outer-right-padding (+ 2 padding)) # fills the space better - add centering option?
  (def left-padding (+ outer-left-padding tick-height))
  (def right-padding outer-right-padding)

  # Draw Y Label
  (when y-label
    (def [w _h] (text-measure y-label font 1))
    (text-draw canvas padding (div (+ height w top-padding (- bottom-padding)) 2) y-label line-color font 1 1))

  # Closure to convert metric space to pixel space - only can be done after full padding calculations
  (def scale-x (/ (- width left-padding right-padding inner-padding inner-padding) orig-dx))
  (def scale-y (- (/ (- height top-padding bottom-padding inner-padding inner-padding) orig-dy)))
  (def offset-x (- left-padding (- inner-padding) (* scale-x x-min)))
  (def offset-y (- height bottom-padding inner-padding (* scale-y y-min)))
  (defn convert
    [metric-x metric-y]
    [(+ offset-x (* scale-x metric-x))
     (+ offset-y (* scale-y metric-y))])

  # TODO - replace tick mark draw calls to g/plot with g/fill-rect to allow for thicker ticks

  # Draw Y axis
  (assert yticks "unable to generate y ticks. Make your chart bigger?")
  (each metric-y yticks
    (def [_ pixel-y] (convert 0 metric-y))
    (def rounded-pixel-y (math/round pixel-y))
    (def text (yformat metric-y))
    (def [text-width] (text-measure text font 1))
    (text-draw canvas (- outer-left-padding text-width 3) (- rounded-pixel-y font-half-height) text line-color font 1)
    (if has-grid
      (g/plot canvas left-padding rounded-pixel-y (- width right-padding) rounded-pixel-y grid-color stipple-cycle stipple-on)
      (g/plot canvas (+ tick-trim outer-left-padding) rounded-pixel-y (+ outer-left-padding tick-height) rounded-pixel-y grid-color)))

  # Draw X axis - allow manual override for x tick marks
  (def [xticks xformat]
    (if x-ticks [x-ticks (if format-x format-x string)]
      (guess-axis-ticks x-min x-max (- width left-padding right-padding) 20 x-labels-vertical font x-prefix x-suffix min-x-spacing format-x)))
  (assert xticks "unable to generate x ticks. Make your chart bigger?")
  (each metric-x xticks
    (def [pixel-x _] (convert metric-x 0))
    (def rounded-pixel-x (math/round pixel-x))
    (def text (xformat metric-x))
    (def [text-width text-height] (text-measure text font 1))
    (if x-labels-vertical
      (text-draw canvas (- rounded-pixel-x -1 (* text-height 0.5)) (- height outer-bottom-padding -3 (- text-width)) text line-color font 1 1)
      (text-draw canvas (- rounded-pixel-x -1 (* text-width 0.5)) (- height outer-bottom-padding -3) text line-color font 1))
    (if has-grid
      (g/plot canvas rounded-pixel-x top-padding rounded-pixel-x (- height bottom-padding) grid-color stipple-cycle stipple-on)
      (g/plot canvas rounded-pixel-x (- height outer-bottom-padding tick-trim) rounded-pixel-x (- height outer-bottom-padding tick-height) grid-color)))

  # Draw minor x tick marks
  (when (and x-minor-ticks (< 1 (length xticks)))
    (def len (length xticks))
    (def dx-first (- (in xticks 1) (in xticks 0)))
    (def dx-last (- (in xticks (- len 1)) (in xticks (- len 2))))
    # we must draw minor ticks before and after the first and last major ticks until the edge of the axis
    (def padded-ticks [(- (in xticks 0) dx-first) ;xticks (+ (in xticks (- len 1)) dx-last)])
    (loop [j :range [1 (length padded-ticks)]
           :let [i (- j 1)
                 t0x (in padded-ticks i)
                 t1x (in padded-ticks j)
                 dx (- t1x t0x)]
           xfloat :range [t0x (+ t1x 0.00001) (/ dx x-minor-ticks)]
           :let [x (math/round (first (convert xfloat 0)))]
           :when (and (> x left-padding) (< x (- width right-padding)))]
      (g/plot canvas x (- height outer-bottom-padding tick-height) x (- height outer-bottom-padding (div tick-height 2)) grid-color)))

  # Draw minor y tick marks
  (when (and y-minor-ticks (< 1 (length yticks)))
    (def len (length yticks))
    (def dy-first (- (in yticks 1) (in yticks 0)))
    (def dy-last (- (in yticks (- len 1)) (in yticks (- len 2))))
    # we must draw minor ticks before and after the first and last major ticks until the edge of the axis
    (def padded-ticks [(- (in yticks 0) dy-first) ;yticks (+ (in yticks (- len 1)) dy-last)])
    (loop [j :range [1 (length padded-ticks)]
           :let [i (- j 1)
                 t0y (in padded-ticks i)
                 t1y (in padded-ticks j)
                 dy (- t1y t0y)]
           yfloat :range [t0y (+ t1y 0.00001) (/ dy y-minor-ticks)]
           :let [y (math/round (get (convert 0 yfloat) 1))]
           :when (and (> y top-padding) (< y (- height bottom-padding)))]
      (g/plot canvas (+ outer-left-padding (div tick-height 2)) y (+ outer-left-padding tick-height) y grid-color)))

  # Draw frame
  (draw-frame canvas left-padding top-padding (- width right-padding) (- height bottom-padding) grid-color 2)

  # Create a cropped view inside our "Frame" that can then be used for rendering charts
  (def frame-width (- width left-padding right-padding))
  (def frame-height (- height top-padding bottom-padding))
  (def view (g/viewport canvas
                        (+ 1 left-padding) (+ 1 top-padding)
                        (- frame-width 1) (- frame-height 1)))
  (def frame-scale-x (/ (- frame-width inner-padding inner-padding) orig-dx))
  (def frame-scale-y (- (/ (- frame-height inner-padding inner-padding) orig-dy)))
  (def frame-offset-x (- inner-padding (* frame-scale-x x-min)))
  (def frame-offset-y (- frame-height -1 inner-padding (* frame-scale-y y-min)))
  (defn view-convert
    [metric-x metric-y]
    [(+ frame-offset-x (* frame-scale-x metric-x))
     (+ frame-offset-y (* frame-scale-y metric-y))])
  (defn view-unconvert
    [pixel-x pixel-y]
    [(/ (- pixel-x frame-offset-x) frame-scale-x)
     (/ (- pixel-y frame-offset-y) frame-scale-y)])

  [view view-convert view-unconvert])

###
### Line Graphs
###

(defn plot-line-graph
  ```
  Plot a line graph or scatter graph on a canvas. This function does not add a set of axis, title, or chart legend, it will only plot the graph lines and points from data.

  * :canvas - a gfx2d/Image to draw on
  * :to-pixel-space - optional function (f x y) -> [pixel-x pixel-y]. Used to convert the metric space to pixel space when plotting points.
  * :data - a data frame to use for x and y data
  * :x-column - the name of the data frame column to use for the x axis
  * :y-column - a single column name or list of column names to use for the y coordinates and connected lines
  * :color-map - a dictionary mapping columns to colors. By default will hash column name to pseudo-random colors
  * :line-style - How to draw lines. Can be one of :stroke, :plot, :none, :bar, :area, or :stipple. Default is :plot.
  * :line-style-per-column - Optional dictionary to override line style by y-column name.
  * :circle-points - add circles around each point
  * :point-radius - how large to make the circles around each point in pixels
  * :super-sample - use super sampling to draw a larger image and then scale it down for anti-aliasing.
  * :bar-padding - space between bars in bar-charts
  * :stroke-thickness - thickness in pixels of the stroke of the graph when :line-type = :stroke
  * :x-colors - for bar and scatter plots, optionally set per-point/per-bar colors with an function (f x y index) called on each point.
  ```
  [&named
   canvas
   data
   to-pixel-space
   line-style
   line-style-per-column
   x-column
   y-column
   circle-points
   point-radius
   x-colors
   bar-padding
   stroke-thickness
   super-sample
   color-map]

  (def {:width canvas-width :height canvas-height} (g/unpack canvas))
  (default to-pixel-space (fn :convert [x y] [x y]))
  (default color-map {})
  (default line-style-per-column {})
  (default line-style :plot)
  (default bar-padding 4)
  (default point-radius 3)
  (default stroke-thickness 1.5)
  (default super-sample 1)

  # Super sampling!
  # Super sampling does not work well with pixel-based line styles, like :plot, :stipple
  # Intended for use with :stroke and :bar.
  (when (> super-sample 1)
    (def new-canvas (g/blank (* super-sample canvas-width) (* super-sample canvas-height)))
    (def temp-canvas (g/blank canvas-width canvas-height))
    (plot-line-graph :canvas new-canvas
                     :to-pixel-space (fn [x y] (def [x1 y1] (to-pixel-space x y)) [(* super-sample x1) (* super-sample y1)])
                     :data data
                     :x-column x-column
                     :y-column y-column
                     :circle-points circle-points
                     :bar-padding (* super-sample bar-padding)
                     :color-map color-map
                     :super-sample nil
                     :stroke-thickness (* super-sample stroke-thickness)
                     :point-radius (* super-sample point-radius)
                     :line-style-per-column line-style-per-column
                     :line-style line-style)
    # The resize + blend must match, as well as the destination pixels!
    # After resize, alpha is pre-multiplied
    (g/resize-into temp-canvas new-canvas true)
    (g/stamp-blend canvas temp-canvas :premul)
    (break canvas))

  # Allow single or multiple y-columns shorthand - draw first column on top
  (def y-columns (if (indexed? y-column) (reverse y-column) [y-column]))

  # Draw graph
  (def xs (get data x-column))
  (assert (indexed? xs))
  (each ycol y-columns
    (def graph-color (get color-map ycol (color-hash ycol)))
    (default x-colors (fn :default-x-colors [&] graph-color))
    (def ys (get data ycol))

    # Collect points - handle missing ys
    (def pts @[])
    (for i 0 (length xs)
      (def x (get xs i))
      (when x
        (def y (get ys i))
        (when y
          (def [x1 y1] (to-pixel-space x y))
          (array/push pts (math/round x1) (math/round y1)))))

    # Plot lines between points
    (def line-style2 (get line-style-per-column ycol line-style))
    (enum line-style2 :plot :stipple :stroke :bar :none :area)
    (case line-style2

      :stipple
      (do
        (def up-pts (array/slice pts))
        (loop [i :range [1 (length pts) 2]]
          (+= (up-pts i) 1))
        (g/plot-path canvas up-pts graph-color 8 5)
        (g/plot-path canvas pts graph-color 8 5))

      :plot
      (do
        (def up-pts (array/slice pts))
        (loop [i :range [1 (length pts) 2]]
          (+= (up-pts i) 1))
        (g/plot-path canvas pts graph-color)
        (g/plot-path canvas up-pts graph-color))

      :stroke
      (do
        (g/stroke-path canvas pts graph-color stroke-thickness))

      :area
      (do
        (def min-x (first pts))
        (def max-x (get pts (- (length pts) 2)))
        (def bottom-y 10000)
        (g/fill-path canvas [;pts max-x bottom-y min-x bottom-y] graph-color))

      :bar
      (do
        (def [base-x base-y] (to-pixel-space 0 0))
        (var last-right nil)
        (loop [i :range [0 (length pts) 2]]
          (def j (div i 2))
          (def is-first (= 0 i))
          (def is-last (= i (- (length pts) 2)))
          (def x (get pts i))
          (def y (get pts (+ 1 i)))
          (def color (x-colors (get xs j) (get ys j) j))
          (def x-next (if-not is-last (get pts (+ i 2))))
          (def x-prev (if-not is-first (get pts (- i 2))))
          # First and last bars extrapolate bar width
          (def x-next1 (if is-last (+ x x (- x-prev)) x-next))
          (def x-prev1 (if is-first (+ x x (- x-next)) x-prev))
          # Prefer to use `last-right` to keep pixel padding consistent. Otherwise, the bars look a little off due to rounding errors.
          (def x-left (if last-right (+ last-right bar-padding) (math/ceil (mean [x x-prev1]))))
          (def x-right (math/floor (mean [x x-next1])))
          (def width (- x-right x-left bar-padding))
          (set last-right (+ x-left width))
          (g/fill-rect canvas x-left base-y width (- y base-y) color))))

    # Plot points
    (when circle-points
      (loop [i :range [0 (length pts) 2]]
        (def x (get pts i))
        (def y (get pts (+ 1 i)))
        (def j (div i 2))
        (def color (x-colors (get xs j) (get ys j) j))
        (case line-style2
          :plot
          (g/plot-ring canvas x y point-radius color)
          :stipple
          (g/plot-ring canvas x y point-radius color)
          (g/ring canvas x y (- point-radius stroke-thickness) point-radius color)))))

  canvas)

(defn line-chart
  ```
  Render a line chart. Returns a gfx2d/Image which can be further manipulated with the spork/gfx2d module.

  Basic Parameters
  * :width - canvas width
  * :height - canvas height
  * :data - a data frame to use for data
  * :title - an optional title to add to the rendered image
  * :font - font used to draw text, including title, legend, and axes labels
  * :save-as - save the generated image to file. Can be any format supported by the gfx2d module
  * :x-column - the name of the data frame column to use for the x axis
  * :y-column - a single column or array of column names to use for the chart
  * :x-ticks - manually set the tick marks on the X axis instead of auto-detecting them

  Axes Styling
  * :inner-padding - the number of pixels of white space between x-min and the x-axes as well as y-min and the y-axes.
  * :x-label - optional label for the x axis
  * :y-label - optional label for the y axis
  * :grid - how to draw grid lines. One of :none, :solid, or :stipple
  * :x-suffix - add a string suffix to each tick label on the x-axis
  * :y-suffix - add a string suffix to each tick label on the x-axis
  * :x-prefix - add a string prefix to each tick label on the y-axis
  * :y-prefix - add a string prefix to each tick label on the y-axis
  * :x-minor-ticks - how many, if any, small ticks to add between each large tick mark on the x axis
  * :y-minor-ticks - how many, if any, small ticks to add between each large tick mark on the y axis
  * :x-labels-vertical - Turn x labels vertical so more can fit on the axis
  * :tick-length - how long to make major tick marks

  Chart Styling
  * :padding - the number of pixels of white space around various elements of the chart
  * :background-color - color of background, defaults to white
  * :text-color - color of text, defaults to black
  * :color-map - a dictionary mapping columns to colors. By default will hash column name to pseudo-random colors
  * :scatter - set to true to disable lines connecting points
  * :legend - set to true to add a legend to the top of the chart
  * :legend-map - a dictionary mapping column names to pretty text for the chart
  * :point-radius - radius of points when drawing a scatter plot
  * :line-style - How to draw lines. Can be one of :stroke, :plot, :none, :bar, :area, or :stipple. Default is :plot.
  * :line-style-per-column - Optional dictionary to override line style by y-column name.
  * :super-sample - Super Sample anti-aliasing for chart lines. Is a bit slow, but makes smooth plots. Works best with :stroke and :bar
  * :stroke-thickness - thickness in pixels of the stroke of the graph when :line-type = :stroke

  Axis Boundaries
  * :x-min - minimum x coordinate on chart
  * :x-max - maximum x coordinate on chart
  * :y-min - minimum y coordinate on chart
  * :y-max - maximum y coordinate on chart
  ```
  [&named
   width height data
   font background-color text-color color-map
   point-radius
   x-min x-max y-min y-max
   padding inner-padding title
   circle-points
   scatter grid legend super-sample stroke-thickness
   format-x format-y
   save-as
   legend-map
   tick-length
   line-style line-style-per-column
   x-label y-label
   x-suffix x-prefix y-suffix y-prefix
   x-column y-column
   x-ticks x-minor-ticks y-minor-ticks
   x-labels-vertical]

  # Check parameters and set defaults.
  (assert x-column)
  (assert y-column)
  (default width default-width)
  (default height default-height)
  (default padding (dyn *padding* default-padding))
  (default point-radius 3)
  (default color-map {})
  (default background-color (dyn *background-color* default-background-color))
  (default text-color (dyn *text-color* default-text-color))
  (default font (dyn *font* default-font))
  (default circle-points false)
  (default grid :none)
  (default line-style :plot)
  (default legend :none)

  # Check enums
  (enum grid :none :solid :stipple)
  (enum line-style :plot :stipple :stroke :bar :none :area) # - allow for dictionary of styles
  (enum legend :none :top :top-left :top-right :bottom-left :bottom-right)

  # Allow variadic shorthand
  (def y-columns (if (indexed? y-column) y-column [y-column]))

  # Get canvas
  (def canvas (g/blank width height 4))
  (g/fill-rect canvas 0 0 width height background-color)

  # Render title section, and update view to cut out title
  (var title-padding 0)
  (when title
    (def title-scale 2)
    (def [title-width title-height] (text-measure title font title-scale))
    (set title-padding (+ padding title-height))
    (text-draw canvas (math/round (* 0.5 (- width title-width))) padding title text-color font title-scale))

  # Add legend if legend = :top. This makes a horizontal legend just below the title with no extra framing
  (def legend-padding (max 4 (div padding 4)))
  (when (= legend :top)
    (+= title-padding (div padding 2))
    (def view-width (- width padding padding))
    (def [lw lh] (draw-legend nil :font font :padding legend-padding :labels y-columns :legend-map legend-map :view-width view-width))
    (def legend-view (g/viewport canvas (math/floor (* (- width lw) 0.5)) title-padding lw lh true))
    (+= title-padding lh)
    (-= title-padding (math/floor (* 0.5 padding))) # just looks a bit better
    (draw-legend legend-view :font font :padding legend-padding :labels y-columns :color-map color-map
                 :legend-map legend-map :text-color text-color :view-width view-width))

  # Crop title section out of place where axis and charting will draw
  (def view (g/viewport canvas 0 title-padding width (- height title-padding)))

  # Draw axes
  (def [x-min x-max y-min y-max] :shadow
    (let [{:width view-width :height view-height} (g/unpack view)]
      (calculate-data-bounds data x-column y-columns
                             view-width view-height 20
                             x-min x-max y-min y-max)))
  (def [graph-view to-pixel-space _to-metric-space]
    (draw-axes view
               :padding padding :inner-padding inner-padding
               :font font
               :grid grid
               :format-x format-x :format-y format-y
               :x-suffix x-suffix :x-prefix x-prefix
               :y-suffix y-suffix :y-prefix y-prefix
               :x-min x-min :x-max x-max
               :y-min y-min :y-max y-max
               :x-ticks x-ticks :tick-length tick-length
               :x-label x-label :y-label y-label
               :x-minor-ticks x-minor-ticks
               :y-minor-ticks y-minor-ticks
               :x-labels-vertical x-labels-vertical))

  # Render graph lines
  (plot-line-graph
    :canvas graph-view
    :to-pixel-space to-pixel-space
    :data data
    :x-column x-column
    :y-column y-columns
    :color-map color-map
    :line-style line-style
    :line-style-per-column line-style-per-column
    :super-sample super-sample
    :circle-points (or circle-points scatter)
    :stroke-thickness stroke-thickness
    :point-radius point-radius)

  # Draw internal legend if selected
  (when (index-of legend [:top-left :top-right :bottom-left :bottom-right])
    (def [lw lh] (draw-legend nil :font font :padding legend-padding :labels y-columns :legend-map legend-map :frame false))
    (def {:width gw :height gh} (g/unpack graph-view))
    (def legend-view
      (case legend
        :top-left (g/viewport graph-view padding padding lw lh true)
        :top-right (g/viewport graph-view (- gw lw padding) padding lw lh true)
        :bottom-left (g/viewport graph-view padding (- gh lh padding) lw lh true)
        :bottom-right (g/viewport graph-view (- gw lw padding) (- gh lh padding) lw lh true)))
    (g/fill-rect legend-view 0 0 lw lh background-color)
    (draw-legend legend-view :font font :padding legend-padding :labels y-columns :view-width 0
                 :color-map color-map :legend-map legend-map :frame true))

  # Save to file
  (when save-as
    (g/save save-as canvas))

  canvas)

###
### Heat Maps
###
### Rather than using a "data-frame" abstraction, we just provide a way to
### provide a function that maps input row and column to a color or value. Such
### a function is usually a oneliner given most reasonable data structures.

(defn plot-heat-map
  ```
  Render a heat map on a set of axis. Will nicely fill the passed in image, so use a subview to draw to a section of the chart.

  Basic Parameters
  * :canvas - A gfx2d/Image to draw on
  * :color-fn - Function `(color-fn x y)` that returns a gfx2d color (32 bit integer) used to color each cell in the heat-map. If color-fn evaluates to a falsey value, that cell will be left blank.
  * :cell-text-fn - Function `(cell-text-fn x y)` that returns an optional string to render for each cell.
  * :num-columns - Number of columns to draw.
  * :num-rows - Number of rows to draw.
  * :box-gap - Number of pixels between boxes on the heat map
  * :font - font used to draw optional text in cells
  * :cell-text-color - color of text, defaults to black or white depending on the cell color

  Returns the modified original canvas.
  ```
  [&named
   canvas
   color-fn
   cell-text-fn
   num-columns
   num-rows
   box-gap
   font
   cell-text-color]

  # Check parameters and set defaults.
  (assert num-columns)
  (assert num-rows)
  (assert color-fn)
  (def {:width canvas-width :height canvas-height} (g/unpack canvas))
  (default box-gap 0)
  (default font (dyn *font* default-font))

  # Calculate box sizes - not always integers!
  (def box-width (- (/ (- canvas-width box-gap) num-columns) box-gap))
  (def box-height (- (/ (- canvas-height box-gap) num-rows) box-gap))
  (loop [y :range [0 num-rows]
         x :range [0 num-columns]
         :let [color (color-fn x y)]
         :when color] # skip empty cells
    (def yflip (- num-rows 1 y))
    # Weird math to keep gap sizes consistent for a nice look when things don't divide perfectly.
    (def pixel-x (math/floor (+ box-gap (* x (+ box-gap box-width)))))
    (def pixel-y (math/floor (+ box-gap (* yflip (+ box-gap box-height)))))
    (def next-pixel-x (math/floor (* (+ 1 x) (+ box-gap box-width))))
    (def next-pixel-y (math/floor (* (+ 1 yflip) (+ box-gap box-height))))
    (g/fill-rect canvas pixel-x pixel-y (- next-pixel-x pixel-x) (- next-pixel-y pixel-y) color)

    # Per cell text
    (when-let [text (and cell-text-fn (cell-text-fn x y))]
      (def [w h] (text-measure text font 1 0))
      (def text-x (math/floor (- (mean [pixel-x next-pixel-x]) (/ w 2))))
      (def text-y (math/floor (- (mean [pixel-y next-pixel-y]) (/ h 2))))
      (def tcolor (or cell-text-color (if (< 0.6 (color-value color)) g/black g/white))) # black or white, maximizing contrast
      (text-draw canvas text-x text-y text tcolor font 1 0)))

  canvas)

(defn heat-map-chart
  ```
  Generate a heat map.

  Render a heat map on a set of axis. Will nicely fill the passed in image, so use a subview to draw to a section of the chart.

  Basic Parameters
  * :width - New canvas width in pixels
  * :height - New canvas height in pixels
  * :color-map - a color map keyword or function used to map numbers 0
  * :save-as - optional path to save the chart

  Function Callback Input
  * :num-columns - Number of columns to draw.
  * :num-rows - Number of rows to draw.
  * :color-fn - Function `(color-fn x y)` that returns a gfx2d color used to color each cell in the heat-map. If color-fn evaluates to a falsey value, that cell will be left blank.
  * :cell-text-fn - Function `(cell-text-fn x y)` that returns an optional string to render for each cell. If the function evaluates to nil, no text will be drawn for that cell.

  Data Frame Input
  * :data - a dataframe table that contains a grid of cell
  * :data-scale - map numeric data to a [0.0, 1.0] range with a scale factor or function. Is the constant 1.0 by default.
  * :xs - a list of x columns - these are keys in `data`
  * :ys - (optional) keys into each column - by default this is just (range num-rows-in-data).

  Axes Styling
  * :grid - how to draw grid lines. One of :none, :solid, or :stipple
  * :x-ticks - manually set the tick marks on the X axis instead of auto-detecting them
  * :x-label - optional label for the x axis
  * :y-label - optional label for the y axis
  * :x-suffix - add a string suffix to each tick label on the x-axis
  * :y-suffix - add a string suffix to each tick label on the x-axis
  * :x-prefix - add a string prefix to each tick label on the y-axis
  * :y-prefix - add a string prefix to each tick label on the y-axis
  * :x-minor-ticks - how many, if any, small ticks to add between each large tick mark on the x axis
  * :y-minor-ticks - how many, if any, small ticks to add between each large tick mark on the y axis
  * :x-labels-vertical - Turn x labels vertical so more can fit on the axis
  * :tick-length - how long to make major tick marks

  Chart Styling
  * :box-gap - Number of pixels between boxes on the heat map. Default is 0.
  * :cell-font - font used to draw optional text in cells
  * :cell-text-color - color of text, defaults to black or white, depending on cell color
  * :font - font used to draw axes
  * :title-font - font used to draw title. Defaults to font.
  * :text-color - color of axes and title text
  * :padding - Number of pixels to separate various elements of the chart
  * :background-color - chart background color
  * :legend - one of :top, :bottom, :left, :right, :top-left, :top-right, :bottom-left, :bottom-right, or :none
  * :legend-labels - an array of evenly-spaced markers to put on the color map legend.
  * :legend-width - width of color map gradient in the legend in pixels
  * :legend-height - height of the color map gradient in the legend in pixels

  Returns a new canvas.
  ```
  [&named
   width height
   data data-scale xs ys
   color-fn cell-text-fn
   num-columns num-rows
   font title-font cell-font
   color-map
   background-color
   text-color cell-text-color
   x-min x-max y-min y-max
   format-x format-y
   padding
   title
   box-gap
   legend legend-frame legend-labels
   x-label y-label
   x-suffix x-prefix y-suffix y-prefix
   x-ticks x-minor-ticks y-minor-ticks tick-length
   x-labels-vertical
   legend-width legend-height
   save-as]

  # Check parameters and set defaults.
  (default width default-width)
  (default height default-height)
  (default padding (dyn *padding* default-padding))
  (default background-color (dyn *background-color* default-background-color))
  (default text-color (dyn *text-color* default-text-color))
  (default font (dyn *font* default-font))
  (default title-font font)
  (default legend :none)
  (default tick-length 0)

  # Allow a few ways to populate the heat-map with data
  (def color-map :shadow (if (keyword? color-map) (assert (get color-maps color-map) "invalid color-map") color-map))
  (default data [[]])
  (default xs (or (and num-columns (range num-columns)) (sort (keys data))))
  (default ys (range (or num-rows (length (get data (first xs))))))
  (default data-scale 1.0)
  (def scale-fn (if (function? data-scale) data-scale (fn [t] (* t data-scale))))
  (defn get-point [x y]
    (def xcol (get data (get xs x)))
    (scale-fn (get xcol y)))
  (default color-fn (fn [x y] (color-map (get-point x y))))
  (default format-x (if num-columns nil (fn [x] (string (get xs x)))))
  (def num-columns (length xs))
  (def num-rows (length ys))

  (enum legend :none :top :top-left :top-right :bottom-left :bottom-right :left :right :top :bottom)

  # Get canvas
  (def canvas (g/blank width height 4))
  (g/fill-rect canvas 0 0 width height background-color)

  # Render title section, and update view to cut out title
  (var title-padding 0)
  (when title
    (def title-scale 2)
    (def [title-width title-height] (text-measure title title-font title-scale))
    (set title-padding (+ padding title-height))
    (text-draw canvas (math/round (* 0.5 (- width title-width))) padding title text-color title-font title-scale))

  # Add legend on outside of chart
  (def legend-padding (max 4 (div padding 4)))
  (var [right-pad left-pad top-pad bottom-pad] [0 0 0 0])
  (when (index-of legend [:top :bottom :left :right])
    (default legend-frame false)
    (def layout (if (index-of legend [:top :bottom]) :h :v))
    (def [lw lh] (draw-heat-legend nil :font font :padding legend-padding :color-map color-map :labels legend-labels :layout layout
                                   :swatch-width legend-width :swatch-height legend-height :frame legend-frame))
    (def legend-view (g/viewport canvas
                                 (case legend
                                   :top (div (- width lw) 2)
                                   :bottom (div (- width lw) 2)
                                   :left padding
                                   :right (- width padding lw))
                                 (case legend
                                   :top title-padding
                                   :bottom (- height padding lh)
                                   :left (div (- height lh (div title-padding -2)) 2)
                                   :right (div (- height lh (div title-padding -2)) 2))
                                 lw lh true))
    (case legend
      :left (set left-pad (+ lw padding))
      :right (set right-pad (+ lw padding))
      :top (set top-pad (+ lh padding))
      :bottom (set bottom-pad (+ lh padding)))
    (draw-heat-legend legend-view :font font :padding legend-padding :color-map color-map :labels legend-labels :layout layout
                      :swatch-width legend-width :swatch-height legend-height :frame legend-frame
                      :text-color text-color))

  # Crop title section and legend padding out of place where axis and charting will draw
  (def view (g/viewport canvas
                        left-pad
                        (+ top-pad title-padding)
                        (- width right-pad left-pad)
                        (- height title-padding top-pad bottom-pad)))

  # Draw axes
  (def {:width view-width :height view-height} (g/unpack view))
  (default x-min -0.5)
  (default y-min -0.5)
  (default x-max (+ -0.5 num-columns))
  (default y-max (+ -0.5 num-rows))
  (def [graph-view to-pixel-space _to-metric-space]
    (draw-axes view
               :padding padding :inner-padding 0
               :font font
               :grid :none # grid doesn't work well with heat-map
               :min-x-spacing 1 :min-y-spacing 1
               :format-x format-x :format-y format-y
               :x-suffix x-suffix :x-prefix x-prefix
               :y-suffix y-suffix :y-prefix y-prefix
               :x-min x-min :x-max x-max
               :y-min y-min :y-max y-max
               :x-ticks x-ticks :tick-length tick-length
               :x-label x-label :y-label y-label
               :x-minor-ticks x-minor-ticks
               :y-minor-ticks y-minor-ticks
               :x-labels-vertical x-labels-vertical))

  # Plot the heat-map
  (plot-heat-map
    :canvas graph-view
    :color-fn color-fn
    :cell-text-fn cell-text-fn
    :num-columns num-columns
    :num-rows num-rows
    :font cell-font
    :cell-text-color cell-text-color
    :box-gap box-gap)

  # Draw internal legend if selected
  (when (index-of legend [:top-left :top-right :bottom-left :bottom-right])
    (default legend-frame true)
    (def legend-layout :v)
    (def [lw lh] (draw-heat-legend nil :font font :padding legend-padding :color-map color-map :labels legend-labels
                                   :layout legend-layout :swatch-width legend-width :swatch-height legend-height :frame legend-frame))
    (def {:width gw :height gh} (g/unpack graph-view))
    (def legend-view
      (case legend
        :top-left (g/viewport graph-view padding padding lw lh true)
        :top-right (g/viewport graph-view (- gw lw padding) padding lw lh true)
        :bottom-left (g/viewport graph-view padding (- gh lh padding) lw lh true)
        :bottom-right (g/viewport graph-view (- gw lw padding) (- gh lh padding) lw lh true)))
    (g/fill-rect legend-view 0 0 lw lh background-color)
    (draw-heat-legend legend-view :font font :padding legend-padding :color-map color-map
                      :swatch-width legend-width :swatch-height legend-height
                      :text-color text-color :labels legend-labels :layout legend-layout :frame legend-frame))

  # Save to file
  (when save-as
    (g/save save-as canvas))

  canvas)
