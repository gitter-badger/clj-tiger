(ns clj-tiger.translate
  (:require [clojure.core.match :refer [match]]
            [clj-tiger.util :as util]))

;;; This module accepts a parse tree from the `parse` module and converts it to an
;;; intermediate representation tree.  During this process it type-checks all variables
;;; and expressions, and converts variables into temporaries.
;;; 
;;; This corresponds to the `semant` modules in the Tiger book.

(declare trans-exp)
(declare trans-type)

(defn trans-dec [v-env t-env sym-env ast]
  (match ast
         ;; Typed Variable Declarations
         [:tvardec the-var the-type body]
         (let [dec-type  (trans-type the-type)
               v-env'    (assoc v-env the-var dec-type)
               t         (gensym "t")
               sym-env'  (assoc sym-env the-var t)
               [ir-type ir-exp] (trans-exp v-env' t-env sym-env' body)]
           (if (= ir-type dec-type)
             [v-env' t-env sym-env' [:move t ir-exp]]
             (throw (Exception. (str "Type error: expected " dec-type " but got " ir-type "\nin: " ast)))))

         ;; Variable Declaraions with no declared type
         [:vardec the-var body]
         (let [t         (gensym "t")
               sym-env'  (assoc sym-env the-var t)
               [ir-type ir-exp] (trans-exp v-env t-env sym-env' body)
               v-env'    (assoc v-env the-var ir-type)] ;; Use the inferred type
           [v-env' t-env sym-env' [:move t ir-exp]])
         
         )) 


(declare trans-rec)
(defn trans-type [t-exp]
  (match t-exp
         "int"               :int
         "string"            :string
         "nil"               :nil
         "unit"              :unit
         [:array t]          [:array (trans-type t)]
         [& r]               (vec (cons :record (cons (gensym "R") (map trans-rec r))))
         (s :guard string?)  [:name s (atom nil)]  ;; This is for mutually recursive types
         x                   (throw (Exception. (str "Type didn't parse: " x)))))

(defn trans-rec [v]
  (let [the-var (first v)
        the-type (second v)]
    [the-var (trans-type the-type)]))

(defn type-error [expected actual exp]
  (throw (Exception. (str "Type error.  Expected " expected
                          " but got " actual " in expression "
                          exp))))

(defn trans-exp [v-env t-env sym-env ast]
  (match ast
         [:int i]    [:int      [:const i]]
         [:varexp v] [(v-env v) (sym-env v)]

         [:ifexp e1 e2 e3]
         (let [[te1 ie1]  (trans-exp v-env t-env sym-env e1)
               [te2 ie2]  (trans-exp v-env t-env sym-env e2)
               [te3 ie3]  (trans-exp v-env t-env sym-env e3)]
           (cond (not= :bool te1)
                 (type-error :bool te1 e1)
                 (not= te2 te3)
                 (type-error te2 te3 e3)
                 :else
                 [te2 [:cjump ie1 ie2 ie3 ]])

           )))

(defn trans-ast [ast]
  (match ast
         [:S & decs]  (reduce (fn [[v-env t-env sym-tab ir-code] [:dec the-dec]]
                                (let [[v-env' t-env' sym-tab' ir-code']  (trans-dec v-env t-env sym-tab the-dec)]
                                  [v-env' t-env' sym-tab' [:seq ir-code ir-code']]))
                              [{} {} {} nil] decs)))

(defn emit-prelude
  "Generates code to load contents of stack into the variables of the
  function.  The variables are assumed to be [:temp] variables in IR code.
  The params should be in order of declaration; emit-prelude will reverse them
  for this.  It also emits code to save the return address [:temp 'r7] and the frame pointer [:temp 'r5]."
  [params]
  [:seq
   (reduce (fn [acc code] [:seq code acc])
           (map-indexed (fn [pos param]
                          [:move param [:mem [:binop :plus [:temp 'r5]] [:const pos]]])
                        (reverse params)))
   [:seq [:move [:temp 'r3] [:binop :minus [:temp 'r3] [:const 2]]] ;; make space on stack for RA and FP
    [:seq [:move [:temp 'r7] [:mem [:binop :add [:temp 'r6] [:const 1]]]]  ;; save old RA
     [:seq [:move [:temp 'r5] [:mem [:binop :add [:temp 'r6] [:const 1]]]]]    ;; save old FP
     [:move [:temp 'r6] [:temp 'r5]]  ;; set new frame pointer.
     ]]])

(defn emit-call
  "Emit IR code to call a function.  Expects the function label and the IR tree
expressions for the arguments.  This will take care of moving the stack pointer
and the frame pointer.  It doesn't actually do the jump though!"
  [label args]
  ;; First push the args
  (map (fn [arg] 
         [:seq
          [:move arg [:mem [:temp 'r6]]]
          [:move [:binop :minus [:temp 'r6] [:const 1]] [:temp 'r6]]])
       (reverse args)))
