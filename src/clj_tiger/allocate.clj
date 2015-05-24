(ns clj-tiger.allocate)

(defn update-default
  "Updates the value of key k in hash h using function f.
If the key is not in the hash it uses d for the value."
  [h k d f]
  (let [v (or (h k) d)]
    (assoc h k (f v))))

(defn make-graph [] {})

(defn add-edge [g a b]
  (-> g
      (update-default a #{} #(conj % b))
      (update-default b #{} #(conj % a))))

(defn remove-edge [g a b]
  (-> g
      (update-default a #{} #(disj % b))
      (update-default b #{} #(disj % a))))

(defn remove-vertex [g v]
  (let [incidents (g v)]
    (dissoc (reduce #(remove-edge %1 v %2) g incidents) v)))

(defn degree
  "Return the degree of vertex a in graph g"
  [g a]
  (count (g a)))

(defn safe-min [& elts]
  (let [elts' (filter identity elts)]
    (if (empty? elts') nil (apply min elts'))))

(defn get-min-degree [g]
  (loop [vs (keys g)
         min-vert nil
         min-degree nil]
    (if (empty? vs) [min-vert min-degree]
        (let [v (first vs)
              d (degree g v)]
          (if (or (nil? min-degree) (< d min-degree))
            (recur (rest v) v d)
            (recur (rest v) min-vert min-degree))))))

(defn reduce-graph
  "Given a graph g and max-degree k, return a stack to use for allocation."
  ([g]
   (loop [vertex-stack '()
          reduced g]
     (if (empty? reduced) vertex-stack
         (let [[min-v min-d] (get-min-degree reduced)]
           (recur 
            (cons [min-v (reduced min-v)] vertex-stack)
            (remove-vertex reduced min-v)))))))

(defn allocate-from-stack
  [stack k]
  (let [registers (into #{} (range k))]
    (loop [colors {}
           g (make-graph)
           stack stack]
      (if (empty? stack) colors
          (let [[v adjs] (first stack)
                used-colors (into #{} (map colors adjs))
                next-color (first (remove used-colors registers))
                new-g (reduce #(add-edge %1 v %2) g adjs)]
            (if (nil? next-color)
              (recur (assoc colors v :spill) new-g (rest stack))
              (recur (assoc colors v next-color) new-g (rest stack))))))))

(defn allocate [g k]
  (allocate-from-stack (reduce-graph g) k))

(def g11-1
  (reduce (fn [g [a b]] (add-edge g a b))
          (make-graph)
          [["f" "e"]
           ["f" "j"]
           ["f" "m"]
           ["e" "j"]
           ["e" "b"]
           ["e" "m"]
           ["j" "k"]
           ["j" "b"] ; move edge
           ["j" "d"]
           ["j" "g"]
           ["k" "b"]
           ["k" "g"]
           ["k" "d"]
           ["b" "m"]
           ["b" "d"]
           ["b" "c"]
           ["m" "c"]
           ["m" "d"]
           ["d" "c"] ; move edge
           ["h" "g"]]))


(defn neato-aux [g colors edge-list]
  (if (empty? g) edge-list
      (let [[v adjs] (first g)]
        (neato-aux (remove-vertex g v) colors
                   (reduce #(conj %1  (str "  " v " -- " %2 ";"))
                           edge-list
                           adjs)))))

(defn neato [g colors fname]
  (spit fname (clojure.string/join "\n"
                                   (concat ["graph foo {\n"]
                                           (mapv #(str "  " % " [label=\"" % " " (colors %) "\"];") (keys g))
                                           (neato-aux g {} [])
                                           ["}"]))))
