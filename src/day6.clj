(ns day6
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]
            [clojure.string :as string])
  (:refer-clojure :exclude [==]))

(defne uniqueo [l u]
  ([[] []])
  ([[h . t] u]
   (conda
     #_[(fresh [ll lll]
        (membero h t)
        (rembero h t ll)
        (uniqueo ll lll)
        (== u lll))]
     [(fresh [ll]
        (conso h ll l)
        (uniqueo ll t))]
     )))

(defn uni [coll]
  (let [h (first coll)
        t (next coll)]
    (if (contains? (into #{} t) h)
      (uni t)
      (cons h (when t (uni t)))
      )))

(uni [1 2 2 1])

(contains? nil 1)

(run* [q]
  #_(conda
    [(membero 1 [1]) (rembero 1 [3 1 2] q)]
    [(== 1 1) (== 2 q)]
    )
  #_(membero 1 [1])
  #_(rembero 0 [1 1 2] q)
  (uniqueo [1 1] q))

