(ns clj-tiger.util)

  (defn abs "Take the absolut value"
    [x]
    (if (< x 0) (- x) x))
