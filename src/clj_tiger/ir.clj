(ns clj-tiger.ir
  (:require [clojure.core.match :refer [match]]))

;; Expressions

(defmacro make-tree [kw con & fields]
  (let [fn-name (symbol (clojure.string/capitalize con))
        h-val (keyword con)
        kw-fields (map #(keyword %) fields)
        kv-pairs (mapcat #(list %1 %2) kw-fields fields)]
    `(defn ~fn-name [~@fields]
       (assoc {~(keyword kw) ~h-val} ~@kv-pairs))))

(make-tree exp const int)
(make-tree exp name label)
(make-tree exp temp temp)
(make-tree exp binop op e1 e2)
(make-tree exp mem e1)
(make-tree exp call e args)
(make-tree exp eseq s e1)

;; Statements

(make-tree stmt exp e1)
(make-tree stmt move e1 e2)
(make-tree stmt jump e1 labels)
(make-tree stmt cjump op e1 e2 l1 l2)
(make-tree stmt seq s1 s2)
(make-tree stmt label label)

;; Binops ...

;; Just use '+, '-, ...

 
(def sv [ (Label "L1")
          (Move "r1" "r2")
          (Move "r1" "r3")
          (Jump (Const 10) #{"L1" "L2"})
          (Label "L2")
          (Move "r1" "r4")
          (Move "r1" "r5")
          (Jump (Const 20) #{"L1" "L2"})])

(defn gen-label []
  (str (gensym "L-")))

(defn get-basic-block
  "Given a sequence of IR statments, collects a single basic block."
  ([ir-seq] (get-basic-block ir-seq []))
  ([ir-seq acc]
   (let [ir (first ir-seq)]
     (cond (empty? ir-seq)                     [ir-seq acc],
           (#{:cjump :jump} (:stmt ir)) [(rest ir-seq) (conj acc ir)]
           (#{:label} (:stmt ir)) [ir-seq acc]
           :fine-be-that-way
             (recur (rest ir-seq) (conj acc ir))))))


