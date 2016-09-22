(ns algos.heap
  (:refer-clojure :exclude [empty]))

;;; One-based indexing everywhere, to keep the math simpler.
;;; Heaps have a nil in the 0-th slot, which is never used.

(defn- parent [i]
  (int (/ i 2)))

(defn- left-child [i]
  (* 2 i))

(defn- right-child [i]
  (inc (* 2 i)))

(defn- root? [i]
  (= i 1))

(defn- aswap [a i j]
  (let [vi (aget a i)]
    (aset a i (aget a j))
    (aset a j vi)
    a))

(defn- in-bounds? [h i]
  (and (>= i 1) (<= i (count h))))

(defn- bubble-up
  ([h]
   (bubble-up h (dec (count h))))
  ([h i]
   (if (root? i)
     h
     (let [val        (aget h i)
           parent-val (aget h (parent i))]
       (if (< val parent-val)
         ;; value at i dominates its parent; bubble it up
         (bubble-up (aswap h i (parent i)) (parent i))
         h)))))

(defn- bubble-down [h i]
  (let [[lci rci] [(left-child i) (right-child i)]
        children  (cond-> [[i (aget h i)]]
                    (in-bounds? h lci) (conj [lci (aget h lci)])
                    (in-bounds? h rci) (conj [rci (aget h rci)]))
        f         #(-> h (aswap i %) (bubble-down %))
        mvi       (apply min-key second children)]
    (condp = (first mvi)
      lci (f lci)
      rci (f rci)
      h)))


(defn empty []
  (array nil))

(defn enqueue [h item]
  (aset h (count h) item)
  (bubble-up h)
  h)

(defn dequeue [h]
  (let [minval (aget h 1)
        last-i (count h)]
    (aset h 1 (.pop h))
    [minval (bubble-down h 1)]))

(defn heap [col]
  (reduce enqueue (empty) col))
