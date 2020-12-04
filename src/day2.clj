(ns day2
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd])
  (:refer-clojure :exclude [==]))


(defne occuro [coll e n]
  ([[] _ 0]
   (== 0 n))
  ([[h . t] _ _]
   (conda
     [(fresh [s]
        (== e h)
        (fd/+ 1 s n)
        (occuro t e s))]
     [(!= e h)
      (occuro t e n)])))

(defn str->int [s]
  (Integer. s))

(defn parse-line [line]
  (let [parts (clojure.string/split line #" ")
        range-parts (clojure.string/split (nth parts 0) #"-")]
    {:from (str->int (nth range-parts 0))
     :to (str->int (nth range-parts 1))
     :what (subs (nth parts 1) 0 1)
     :where (vec (map str (nth parts 2)))}))


(defn correct? [{from :from to :to what :what where :where}]
  (not
    (nil?
      (seq
        (run* [q]
          (fd/in q (fd/interval from to))
          (occuro where what q))))))

#_(correct? (parse-line (second (clojure.string/split-lines (slurp "resources/day2.txt")))))

(defn day2a []
  (let [data (map parse-line (clojure.string/split-lines (slurp "resources/day2.txt")))]
    (->> data
      (map correct?)
      (filter identity)
      count)))

(comment
  (day2a))

(defne ntho [coll e n]
  ([[] _ _]
   u#)
  ([[h . t] _ _]
   (conde
     [(== e h)
      (== n 0)]
     [(fresh [s]
        (fd/+ 1 s n)
        (ntho t e s))])))

(defn correct2? [{from :from to :to what :what where :where}]
  (let [from-cnt (count
                   (run* [q]
                     (ntho where what (dec from))))
        to-cnt (count
                 (run* [q]
                   (ntho where what (dec to))))]
    (= 1 (bit-xor from-cnt to-cnt))))


(defn day2b []
  (let [data (map parse-line (clojure.string/split-lines (slurp "resources/day2.txt")))]
    (->> data
      (map correct2?)
      (filter identity)
      count)))

(comment
  (day2b))