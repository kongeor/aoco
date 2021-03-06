(ns day5
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]
            [clojure.string :as string])
  (:refer-clojure :exclude [==]))


(def sample "FBFBBFFRLR")

(defne sumo [coll sum]
  ([[] 0])
  ([[h . t] _]
   (fresh [s]
     (fd/+ h s sum)
     (sumo t s))))

(defn f1 [d s]
  (conde
    [(== d \F) (== s 127)]
    [(== d \B) (== s 0)]))

(defn f2 [d s]
  (conde
    [(== d \F) (== s 63)]
    [(== d \B) (== s 0)]))

(defmacro fr [d s v]
  `(conde
    [(== ~d \B) (== ~s ~v)]
    [(== ~d \F) (== ~s 0)]))

(defmacro fc [d s v]
  `(conde
     [(== ~d \R) (== ~s ~v)]
     [(== ~d \L) (== ~s 0)]))

(defne boardingo [coll sum]
  ([[x1 x2 x3 x4 x5 x6 x7 x8 x9 x10] _]
   (fresh [s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 sr sc st]
     (fr x1 s1 64)
     (fr x2 s2 32)
     (fr x3 s3 16)
     (fr x4 s4 8)
     (fr x5 s5 4)
     (fr x6 s6 2)
     (fr x7 s7 1)
     (fc x8 s8 4)
     (fc x9 s9 2)
     (fc x10 s10 1)
     (sumo [s8 s9 s10] sc)
     (sumo [s1 s2 s3 s4 s5 s6 s7] sr)
     (fd/* sr 8 st)
     (fd/+ sc st sum))))

(defn parse-line [line]
  (first (run* [q]
           (boardingo (vec line) q)
           #_(resto '() q)
           #_(sumo [1 2 q] 10))))

(comment
  ;; TODO go insane ... use only core logic for this
  (apply max (map parse-line (string/split-lines (slurp "resources/day5.txt")))))

(def data (mapv parse-line (string/split-lines (slurp "resources/day5.txt"))))

(comment
  ;; part b
  (let [min (apply min data)
        max (apply max data)
        all (into #{} (range min (inc max)))]
    (clojure.set/difference all (into #{} data))
    ;; TODO check again why this doesn't work
    #_(sort (filter #(not (contains? data %)) all))))

