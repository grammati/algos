(ns algos.heap
  (:refer-clojure :exclude [empty]))


(defn- root? [i]
  (zero? i))

(defn- parent [i]
  {:pre [(not (root? i))]}
  (int (/ (dec i) 2)))

(defn- left-child [i]
  (inc (* 2 i)))

(defn- right-child [i]
  (inc (left-child i)))

(defn- exch! [v i j]
  (assoc v i (v j) j (v i)))

(defn- in-bounds? [h i]
  (and (>= i 0) (< i (count h))))

(defn- bubble-up
  "Bubble up the element at index i in the heap h, restoring the heap
  invariant."
  ([h]
   (bubble-up h (dec (count h))))
  ([h i]
   (if (root? i)
     h
     (let [val        (h i)
           parent-val (h (parent i))]
       (if (< val parent-val)
         ;; value at i dominates its parent; bubble it up
         (bubble-up (exch! h i (parent i)) (parent i))
         h)))))

(defn- bubble-down [h i]
  (let [[lci rci] [(left-child i) (right-child i)]
        children  (cond-> [[i (h i)]]
                    (in-bounds? h lci) (conj [lci (h lci)])
                    (in-bounds? h rci) (conj [rci (h rci)]))
        f         #(-> h (exch! i %) (bubble-down %))
        mvi       (apply min-key second children)]
    (condp = (first mvi)
      lci (f lci)
      rci (f rci)
      h)))

(defn enqueue [h item]
  (bubble-up (conj h item)))

(defn hpeek [h]
  (h 0))

(defn hpop [h]
  {:post [#(= (count %) (dec (count h)))]}
  (case (count h)
    0 (throw (ex-info "Attempt to pop empty heap" {:heap h}))
    1 []
    (let [last-v (h (dec (count h)))]
      (-> h
          pop
          (assoc 0 last-v)
          (bubble-down 0)))))

(defn dequeue [h]
  [(hpeek h) (hpop h)])

(defn heap [col]
  (reduce enqueue [] col))

(defn ->vec [h]
  (loop [h h v []]
    (if (empty? h)
      v
      (let [[minval h] (dequeue h)]
        (recur h (conj v minval))))))
