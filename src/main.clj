(ns main
  (:require [clojure.string :as string]
            [clojure.math :as math]
            [clojure.pprint :as pprint]
            [clojure.inspector :as insp]))

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

(defn assoc-counts [N [[c1 c2] cnt]]
  (assoc-in N [(c->i c1) (c->i c2)] cnt))

(defonce N (let [char-count (count c->i)
                 zeroes (->> (repeat char-count 0)
                             (vec)
                             (repeat char-count)
                             (vec))]
             (reduce assoc-counts zeroes bigram->count)))


(comment

  (defn neg [n] (* -1 n))

  (sort-by (comp neg last) bigram->count)

  (let [sum (apply + (nth N 0))]
    (mapv #(/ % sum) (nth N 0)))

  )
