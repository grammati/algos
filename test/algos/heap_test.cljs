(ns algos.heap-test
  (:require [algos.heap :as heap]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]))

(def check-heap-min
  (prop/for-all
   [v (gen/vector gen/int)]
   (let [h          (heap/heap v)
         [minval h] (heap/dequeue h)]
     (= minval (apply min v)))))

(tc/quick-check 100 check-heap-min)
