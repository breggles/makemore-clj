(ns main
  (:require [clojure.string :as string]
            [clojure.math :as math]
            [clojure.pprint :as pprint]
            [clojure.inspector :as insp]
            [micrograd :as mg]))

(defn- debug [x] (print x) x)

(defonce words (->> (slurp "./resources/names.txt")
                    (#(string/split % #"\n"))))

(defn bigrams [word]
  (let [chs (str \. word \.)]
    (mapv vector chs (drop 1 chs))))

(defonce bigram->count (-> (mapcat bigrams words)
                           (frequencies)))

(defonce characters (set (string/join words)))

(defonce c->i (-> (into {} (mapv vector characters (iterate inc 1)))
                  (assoc \. 0)))

(defonce i->c (clojure.set/map-invert c->i))

(defn assoc-counts [N [[c1 c2] cnt]]
  (assoc-in N [(c->i c1) (c->i c2)] cnt))

(defonce N (let [char-count (count c->i)
                 init (->> (repeat char-count 1)
                           (vec)
                           (repeat char-count)
                           (vec))]
             (reduce assoc-counts init bigram->count)))

(defn normalize-row [row]
  (let [row-sum (apply + row)]
    (mapv #(/ % row-sum) row)))

(defonce P (mapv normalize-row N))

(defn index-greater-than [n prev k v]
  (let [curr (+ prev v)]
    (if (< n curr)
      (reduced k)
      curr)))

(defn pick-likely-index [row]
  (reduce-kv (partial index-greater-than (rand)) 0 row))

(defn generate-name []
  (->> (iterate #(pick-likely-index (nth P %)) 0)
       (drop 1)
       (take-while #(not= 0 %))
       (map i->c)
       (apply str)))

(defn neg [n] (* -1 n))

(defn loss [& words]
  (let [bgrms (mapcat bigrams words)]
    (->> bgrms
         (mapv #(mapv c->i %))
         (mapv (partial get-in P))
         (mapv math/log)
         (apply +)
         (neg)
         (#(/ % (count bgrms))))))

;; neural net

(defn mg-one-hot [size n]
  (assoc (vec (repeat size (mg/const 0))) n (mg/const 1)))

(defn mg-dot [v1 v2]
  (apply mg/add (mapv mg/mul v1 v2)))

(defn transpose [m]
  (apply map vector m))

(defn- dims [m]
  ((juxt count (comp count first)) m))

(defn matrix-mul [m1 m2]
  (when (not= (count (first m1)) (count m2))
    (throw (ex-info "Wrong dimensions" {:dims-m1 (dims m1) :dims-m2 (dims m2)})))
  (->> (for [v1 m1 v2 (transpose m2)]
         (mg-dot v1 v2))
       (partition (count (first m2)))
       (mapv vec)
       (vec)))

(defn init-neurons [x y]
  (vec
    (repeatedly x
                (fn []
                  (vec
                    (repeatedly y #(mg/const (- (rand 8) 4))))))))

(defn matrix-map [f m]
  (mapv #(mapv f %) m))

(defn mg-normalize-row [row]
  (let [row-sum (apply mg/add row)]
    (mapv #(mg/div % row-sum) row)))

(defn- shape [x]
  (if (coll? x)
    (vec (cons (count x) (shape (first x))))
    []))

(defn mg-exp [a]
  (mg/exp a))

(defn bigram-indexes [words]
  (->> words
       (mapcat bigrams)
       (mapv #(mapv c->i %))
       transpose))

(defn entries [coords probs]
  (mapv (partial get-in probs)
        coords))

(defn actual-next-probs [ys probs]
  (entries (mapv vector (range) ys)
           probs))

(defn mg-mean [xs]
  (mg/div (apply mg/add xs)
          (mg/const (count xs))))

(defn mg-soft-max [xs]
  (->> xs
       (mapv mg/log)
       (mg-mean)))

(defn apply-inputs [neurons inputs]
  (let [hots (mapv (partial mg-one-hot 27) inputs)]
    (matrix-mul hots neurons)))

(defn mg-loss [ys outputs]
  (->> outputs
       (matrix-map mg-exp)
       (mapv mg-normalize-row)
       (actual-next-probs ys)
       (mg-soft-max)
       (mg/neg)))


(comment

  (pprint/pprint (mg/backward! (mg/forward! (mg-exp (mg-dot [(mg/const 1) (mg/const 2)] [(mg/const 3) (mg/const 4)])))))

  (shape [[[1 2] [3 4]] [[5 6] [7 8]] [[5 6] [7 8]]])
  (shape [[1 2] [3 4] [5 6]])
  (shape [1 2])
  (shape 3)

  (def bgis (bigram-indexes (take 1 words)))

  (def neurons (init-neurons 27 27))

  (def loss
    (->> (apply-inputs neurons (first bgis))
         (mg-loss (second bgis))))

  @(:val* (mg/forward! loss))

  (mg/zero! loss)

  (mg/backward! loss)

  (second (first neurons))

  (matrix-map #(when (not= 0 @(:grad* %)) (print %)) neurons)

  (def tmp-neurs (init-neurons 2 2))

  (pprint/pprint tmp-neurs)

  (range (count (:kids (mg/forward! (mg/exp (mg/const 3))))))

  (mg/backward! (mg/forward! (mg/add (mg/const 1) (mg/const 2) (mg/const 3))))

  (def a (->>
    (matrix-mul
     (mapv (partial mg-one-hot 2) [0 1])
     tmp-neurs)
    (matrix-map mg-exp)
    (matrix-map mg/forward!)
    (matrix-map mg/backward!)
    ; (:grad*)
    ; (pprint/pprint)
    ))

  (get-in (ffirst a) [:kids])
  (let [bgrms (mapcat bigrams (take 1 words))
        neuron (vec
                (repeatedly 27
                            (fn []
                              (vec
                               (repeatedly 27 #(- (rand 8) 4))))))]
    (->> bgrms
         (mapv #(mapv c->i %))
         (mapv first)
         (mapv (partial one-hot 27))
         (#(matrix-mul % neuron))
         (dims)
         ; (pprint/pprint)
         ))

  (try
    (matrix-mul [[1 2] [3 4]] [[5 6]])
    (catch RuntimeException e
      (pprint/pprint e)))

  (matrix-mul [[1 2] [3 4] [5 6]] [[1 2 3] [4 5 6]])

  (matrix-mul [[1 2 3] [4 5 6]] [[1 2] [3 4] [5 6]])

  (matrix-mul [[1 2]
               [4 5]]
              [[1 2]
               [3 4]])

  (transpose [[1 2 3] [4 5 6]])

  (dot [1 2] [3 4])

  (one-hot 27 13)

  (mg/const 3)

  (sort-by (comp neg last) bigram->count)

  (count (filter (partial = 0) (repeatedly 100 (fn [] (pick-likely-index [0.6 0.3 0.1])))))

  (i->c (pick-likely-index (nth P 0)))

  (repeatedly 10 generate-name)

  (->> (mapv #(mapv math/log %) P)
       (flatten)
       (apply +)
       (neg)
       (#(/ % (* 27 27))))

  (apply loss words)

  (loss "andrej")

  (loss "andrejq")

  )
