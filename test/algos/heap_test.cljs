(ns algos.heap-test
  (:require [algos.heap :as heap]
            [cljs.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            [clojure.test.check.clojure-test :refer-macros [defspec]]))


(defspec heapsort
  100
  (prop/for-all
   [v (gen/vector gen/int)]
   (let [h (heap/heap v)
         w (heap/->vec h)]
     (= w (sort v)))))
