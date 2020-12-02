(ns day1
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd])
  (:refer-clojure :exclude [==]))

(defn str->int [s]
  (Integer. s))

(def data [1721
           979
           366
           299
           675
           1456])

(defne combo2 [coll x y]
  ([[] _ _ ]
   u#)
  ([[h . t]  _ _]
   (conde
     [(== h x)
      (membero y t)]
     [(combo2 t x y)])))

(defn day1a []
  (let [data (map str->int (clojure.string/split-lines (slurp "resources/day1.txt")))]
    (run* [z]
      (fresh [x y]
        (fd/* x y z)
        (fd/+ x y 2020)
        (combo2 data x y)))))

(comment
  (day1a))

(defne combo3 [coll x y z]
  ([[] _ _ _]
   u#)
  ([[h . t] _ _ _]
   (conde
     [(== h x)
      (combo2 t y z)
      #_(plus3 x y z 2020)]
     [(combo3 t x y z)])))

(defn plus3 [x y z s]
  (fresh [s']
    (fd/+ x y s')
    (fd/+ s' z s)))

(defn mul3 [x y z s]
  (fresh [s']
    (fd/* x y s')
    (fd/* s' z s)))

(defn day1b []
  (let [data (map str->int (clojure.string/split-lines (slurp "resources/day1.txt")))]
    (run* [s]
      (fresh [x y z]
        (combo3 data x y z)
        (plus3 x y z 2020)
        (mul3 x y z s)))))

(comment
  ;; please be patient
  ;; this takes about 22s here
  (time (doall (day1b))))