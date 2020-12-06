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

(-->e wso
  ([" "] wso)
  ([\space] wso)
  ([]))

(-->e nlo
  ([\newline] nlo)
  ([]))

(def digits (into #{} "1234567890"))
(defn cr [c1 c2]
  (map char (range (int c1) (inc (int c2)))))               ;; TODO correction of samples
(def alpha (into #{} (concat (cr \a \z) (cr \A \Z))))
(def hex (into #{} (concat (cr \a \f) (cr \0 \9))))
(def color (into #{} (concat hex [\#])))

(def allchars (into #{} (concat (cr \a \z) (cr \A \Z) (cr \0 \9) [\#])))


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

(def-->e allcharo [x]
  ([_] [x]
   (!dcg
     (project [x]
       (== (contains? allchars x) true)))))

(def-->e allcharso [x]
  ([[?d . ?ds]] (allcharo ?d) (allcharso ?ds))
  ([[?d]] (allcharo ?d)))

#_(run 1 [q]
  (allcharso q (vec "#123z") []))


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
  ([[:in]] [\i] [\n])
  ([[:na]] []))

#_(run 1 [q]
  (macroexpand (sizeo q (vec "cm") [])))

(def-->e expro [e]
  ([[:ecl ?x]] [\e] [\c] [\l] [\:] (allcharso ?x))
  ([[:pid ?x]] [\p] [\i] [\d] [\:] (allcharso ?x))
  ([[:eyr ?x]] [\e] [\y] [\r] [\:] (numo ?x))
  ([[:hcl ?x]] [\h] [\c] [\l] [\:] (allcharso ?x))
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
  #_(nth data 8)
  #_(parse-line (nth data 8))
  (def parsed-data (mapv first (map parse-line data))))

#_(run* [s]
  (typeo s '[eyr] []))



#_(run 1 [s]
  (exprso s (vec "123 123") []))

(comment
  d
  #_(day2b))

(defn valido [p]
  (membero [:ecl (lvar)] p)
  (membero [:pid (lvar)] p)
  (membero [:eyr (lvar)] p)
  (membero [:hcl (lvar)] p)
  (membero [:iyr (lvar)] p)
  (membero [:hgt (lvar) (lvar)] p))

(defn valid? [passport]
  (let [p (-> passport second)]
    (= 1
      (count
        (run* [q]
          ;; TODO why doesn't work with valido?
          (membero [:byr (lvar)] p)
          (membero [:ecl (lvar)] p)
          (membero [:pid (lvar)] p)
          (membero [:eyr (lvar)] p)
          (membero [:hcl (lvar)] p)
          (membero [:iyr (lvar)] p)
          (membero [:hgt (lvar) (lvar)] p)
          )))))

  (comment
  #_(map (fn [p] {:data p :valid? (valid? p)}) parsed-data)
  (count (filter identity (map valid? parsed-data))))

#_(valid? (parse-line "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"))


(comment
  (let [input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
        data (map #(clojure.string/replace % "\n" " ") (clojure.string/split input #"\n\n"))
        parsed-data (mapv #(-> % first) (mapv parse-line data))
        wat (second (parsed-data 3))]
    #_wat
    (valid? wat)
    #_(run* [q]
      (valido wat))
    #_(run* [q]
      (valido (-> (nth parsed-data 3) second)))
    #_(map valid? parsed-data)))

(comment
  (let [p '([:hcl (\# \c \f \a \0 \7 \d)]
            [:eyr (\2 \0 \2 \5)]
            [:pid (\1 \6 \6 \5 \5 \9 \6 \4 \8)]
            [:iyr (\2 \0 \1 \1)]
            [:ecl (\b \r \n)]
            (:hgt (\5 \9) :in))]
    (run* [q]
      (membero [:byr (lvar)] p)
      (membero [:ecl (lvar)] p)
      (membero [:pid (lvar)] p)
      (membero [:eyr (lvar)] p)
      (membero [:hcl (lvar)] p)
      (membero [:iyr (lvar)] p)
      (membero [:hgt (lvar) (lvar)] p)
      )))
