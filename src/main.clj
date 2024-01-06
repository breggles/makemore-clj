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

(defn one-hot [size n]
  (assoc (vec (repeat size 0)) n 1))

(defn dot [v1 v2]
  (apply + (map * v1 v2)))

(defn transpose [m]
  (apply map vector m))

(defn matrix-mul [m1 m2]
  (if (not= (count (first m1)) (count m2))
    (throw (ex-info "Wrong dimensions")))
  (->> (for [v1 m1 v2 (transpose m2)]
         (dot v1 v2))
       (partition (count (first m2)))
       (map vec)
       (vec)))

(comment

  (let [bgrms (mapcat bigrams (take 1 words))
        neuron (vec (repeatedly 27 #(vector (- (rand 8) 4))))]
    (->> bgrms
         (mapv #(mapv c->i %))
         (mapv first)
         (mapv (partial one-hot 27))
         (#(matrix-mul % neuron))
         (pprint/pprint)
         ))

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
