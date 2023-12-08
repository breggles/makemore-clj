(ns main
  (:require [clojure.string :as string]
            [clojure.math :as math]
            [clojure.inspector :as insp]))

(def words (->> (slurp "./resources/names.txt")
                (#(string/split % #"\n"))))

(defn bigrams [word]
  (let [chs (conj (vec (cons "<S>" word)) "<E>")]
    (map vector chs (drop 1 chs))))

(->> words
     (take 2)
     (mapcat bigrams))
