(ns day4
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd])
  (:use [clojure.core.logic.dcg])
  (:refer-clojure :exclude [==]))

#_(def data (map #(clojure.string/replace % "\n" " ") (clojure.string/split (slurp "resources/day4.txt") #"\n\n")))

(def data (map #(clojure.string/replace % "\n" " ") (clojure.string/split (slurp "resources/day4.txt") #"\n\n")))

#_(def data (let [lines (-> (slurp "resources/day4.txt") (clojure.string/split #"\n\n"))]
            (->> lines
              (map #(clojure.string/replace % "\n" " "))
              (take 1)
              (clojure.string/join "\n"))))

;; data is 291 lines

(comment
  (take 5 data)
  )

(def d (first data))

(print d)

(-->e wso
  ([" "] wso)
  ([\space] wso)
  ([]))

(-->e nlo
  ([\newline] nlo)
  ([]))

(def digits (into #{} "1234567890"))
(defn cr [c1 c2]
  (map char (range (int c1) (inc (int c2)))))
(def alpha (into #{} (concat (cr \a \z) (cr \A \Z))))
(def hex (into #{} (concat (cr \a \f) (cr \0 \9))))
(def color (into #{} (concat hex [\#])))

(def-->e digito [x]
  ([_] [x]
   (!dcg
     (project [x]
       (== (contains? digits x) true)))))

(def-->e numo [x]
  ([[?d . ?ds]] (digito ?d) (numo ?ds))
  ([[?d]] (digito ?d)))

(def-->e alpho [x]
  ([_] [x]
   (!dcg
     (project [x]
       (== (contains? alpha x) true)))))

(def-->e wordo [x]
  ([[?d . ?ds]] (alpho ?d) (wordo ?ds))
  ([[?d]] (alpho ?d)))

(def-->e coloro [x]
  ([_] [x]
   (!dcg
     (project [x]
       (== (contains? color x) true)))))

(def-->e colorso [x]
  ([[?d . ?ds]] (coloro ?d) (colorso ?ds))
  ([[?d]] (coloro ?d)))

#_(run 1 [q]
  (colorso q (vec "123") []))


(def-->e sepo [s]
  ([[:sep]] [":"]))

(def-->e typeo [s]
  ([[:t 'eyr]] ['eyr])
  ([[:t 'hcl]] ['hcl]))

(def-->e fieldo [f]
  ([[:f ?t]] (typeo ?t)))

; ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
; byr:1937 iyr:2017 cid:147 hgt:183cm

(def-->e sizeo [s]
  ([[:cm]] [\c] [\m])
  ([[:in]] [\i] [\n]))

(def-->e expro [e]
  ([[:ecl ?x]] [\e] [\c] [\l] [\:] (wordo ?x))
  ([[:pid ?x]] [\p] [\i] [\d] [\:] (colorso ?x))
  ([[:eyr ?x]] [\e] [\y] [\r] [\:] (numo ?x))
  ([[:hcl ?x]] [\h] [\c] [\l] [\:] (colorso ?x))
  ([[:byr ?x]] [\b] [\y] [\r] [\:] (numo ?x))
  ([[:iyr ?x]] [\i] [\y] [\r] [\:] (numo ?x))
  ([[:cid ?x]] [\c] [\i] [\d] [\:] (numo ?x))
  ([[:hgt ?x . ?xs]] [\h] [\g] [\t] [\:] (numo ?x) (sizeo ?xs)))

(def-->e exprso [exs]
  ([[?e . ?es]] wso (expro ?e) wso (exprso ?es))
  ([[]] []))

(def-->e passporto [p]
  ([[:passport ?p]] (exprso ?p)))

(def-->e datao [exs]
  ([[?e . ?es]] (exprso ?e) nlo (datao ?es))
  ([[]] []))

(defn parse-line [line]
  (run 1 [s]
    #_(coloro s (vec "fff") [])
    #_(exprso s (take 1 (map vec data)) [])
    #_(exprso s (vec "eyr:123") [])
    #_(exprso s (vec "eyr:123 hcl:#602927") [])
    #_(exprso s (vec "hcl:#111fff") [])
    #_(exprso s (vec "eyr:12 hcl:#60") [])
    #_(datao s (vec "eyr:123 hcl:#602927\neyr:123 hcl:#602927") [])
    (passporto s (vec line) [])
    #_(datao s (vec data) [])
    ))

(comment
  #_(nth data 2))

(comment
  (nth data 8)
  #_(parse-line (nth data 8))
  #_(map parse-line (take 2 (drop 7 data))))

#_(run* [s]
  (typeo s '[eyr] []))



#_(run 1 [s]
  (exprso s (vec "123 123") []))

(comment
  d
  #_(day2b))