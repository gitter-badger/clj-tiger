(ns clj-tiger.liveness)

(def prog
  [{:use #{}       :def #{'a}  :succ #{1}}
   {:use #{'a}     :def #{'b 'd}  :succ #{2}}
   {:use #{'b 'c 'd}  :def #{'c}  :succ #{3}}
   {:use #{'b}     :def #{'a}  :succ #{4}}
   {:use #{'a}     :def #{}    :succ #{1 5}}
   {:use #{'c}     :def #{}    :succ #{}}])

(defn iterate [prog in out]
  (loop [pl (seq prog)
         i  0
         nu-in in
         nu-out out]
    (if (empty? pl)
      [nu-in nu-out]
      (recur (rest pl)
             (inc i)
             (assoc nu-in i
                    (into (:use (prog i))
                          (remove (:def (prog i))
                                  (nu-out i))))
             (assoc nu-out i
                    (reduce into #{} (map #(nu-in %)
                                          (:succ (prog i)))))))))

(defn start [prog]
  (loop [in (vec (repeat (count prog) #{}))
         out in]
    (let [[nu-in nu-out] (iterate prog in out)]
      (if (and (= in nu-in)
               (= out nu-out))
        [nu-in nu-out]
        (recur nu-in nu-out)))))
