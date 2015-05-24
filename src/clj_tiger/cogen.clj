(ns clj-tiger.cogen
  (:require [clojure.core.match :refer (match)]))

(defn conjcat [v1 v2]
  (reduce conj v1 v2))

(defn vflatten [& vs]
  (reduce conjcat (vec vs)))

(defn ir-to-lc3
  "Takes an IR tree and returns a pair of the register stored to along with the LC-3 code."
  [ir]
  
  (match ir
         [:temp v]
         [[:temp v] []]

         [:move [:temp t1] [:binop :plus e2 [:const c3]]]
         (let [[v2r v2c] (ir-to-lc3 e2)]
           [[:temp t1]
            (conj v2c
                  {:op :add
                   :dr [:temp t1]
                   :sr1 v2r
                   :imm5 c3})])

         [:move [:temp t1] [:binop :plus e2 e3]]
         (let [[v2r v2c] (ir-to-lc3 e2)
               [v3r v3c] (ir-to-lc3 e3)
               v1r       [:temp t1] ]
           [v1r
            (conj (conjcat v2c v3c)
                  {:op :add
                   :dr [:temp t1]
                   :sr1 v2r
                   :sr2 v3r})])

         ;; After here are IR trees

         [:cjump ir-op e1 e2 ltrue lfalse]
         (let [[v1r v1c] (ir-to-lc3 e1)
               [v2r v2c] (ir-to-lc3 e2)
               op        ({:lt :brn
                           :gt :brp
                           :lte :brnz} ir-op)
               v3r       [:temp (gensym "t")]]
           [v3r
            (vflatten v1c v2c
                      [{:op :not :dr v2r :sr v2r}
                       {:op :add :dr v2r :sr1 v2r :imm5 1}
                       {:op :add :dr v3r :sr1 v1r :sr2 v2r}
                       {:op op   :pcoffset9 ltrue}
                       {:op :br  :pcoffset9 lfalse}])])

         [:binop :plus e2 [:const c3]]
         (let [[v2r v2c] (ir-to-lc3 e2)
               reg [:temp (gensym "t")]]
           [reg
            (conj v2c
                  {:op :add
                   :dr reg
                   :sr1 v2r
                   :imm5 c3})])

         [:binop :plus e2 e3]
         (let [[v2r v2c] (ir-to-lc3 e2)
               [v3r v3c] (ir-to-lc3 e3)
               reg [:temp (gensym "t")]]
           [reg
            (conj (conjcat v2c v3c)
                  {:op :add
                   :dr reg
                   :sr1 v2r
                   :sr2 v3r})])

         ))
