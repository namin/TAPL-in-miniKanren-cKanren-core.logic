(ns tapl.stlc_deriv_tex
  (:use [tapl.stlc]
        [tapl.stlc_deriv])
  (:use [logically.art.interpreters.meta])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(defn typ-case [c]
  (second c))
(defn typ-env [c]
  (nth c 2))
(defn typ-term [c]
  (nth c 3))
(defn typ-type [c]
  (nth c 4))
(defn proof-head [d]
  (first d))
(defn proof-tail [d]
  (nth d 2))

(defn tex-stlc-type [c]
  (cond
    (symbol? c)
    (do
      (print "{")
      (when (.startsWith (str c) "_") (print "T"))
      (print c)
      (print "}"))    
    (= (first c) :=>)
    (do 
      (print "{\\arr ")
      (tex-stlc-type (nth c 1))
      (print " ")
      (tex-stlc-type (nth c 2))
      (print "}"))))

(defn tex-stlc-term [c]
  (cond
    (symbol? c)
    (do
      (print "{")
      (print c)
      (print "}"))    
    (= (first c) 'fn)
    (do
      (print "{\\fn ")
      (tex-stlc-term (first (nth c 1)))
      (print " ")
      (tex-stlc-term (nth c 2))
      (print "}"))
    (= (count c) 2)
    (do
      (print "{\\app ")
      (tex-stlc-term (first c))
      (print " ")
      (tex-stlc-term (second c))
      (print "}"))))

(defn tex-stlc-env [e]
  (print "{\\env{")
  (doseq [b e]
    (print "{\\bind ")
    (tex-stlc-term (first b))
    (print " ")
    (tex-stlc-type (second b))
    (print "}")
    (print "\\;"))
  (print "}}"))

(declare tex-stlc-proof-tree-sub)

(defn tex-stlc-proof-tree-one [d]
  (let [c (proof-head d)
        tail (proof-tail d)]
    (doseq [other tail]
      (tex-stlc-proof-tree-sub other))
    (print "\\justifies \\typ ")
    (tex-stlc-env (typ-env c))
    (print " ")
    (tex-stlc-term (typ-term c))
    (print " ")
    (tex-stlc-type (typ-type c))
    (println "")
    (println "\\using \\textsc{" (typ-case c) "}")))

(defn tex-stlc-proof-tree-sub [d]
  (println "\\[")
  (tex-stlc-proof-tree-one d)
  (println "\\]"))

(defn tex-stlc-proof-tree-top [ds]
  (println "\\prooftree")
  (tex-stlc-proof-tree-one (first ds))
  (println "\\endprooftree"))

(defn -main []

  (spit
    "tex/ex1.tex"
    (with-out-str
      (tex-stlc-proof-tree-top
        (read-string
          (prn-str
            (first
              (run* [q]
                (fresh [c t d]
                  (nom/fresh [x]
                    (typingo-deriv ['typingo c () `(~'fn ~(nom/tie x x)) t] d)
                    (== q d))))))))))

  (spit
    "tex/ex2.tex"
    (with-out-str
      (tex-stlc-proof-tree-top
        (read-string
          (prn-str
            (first
              (run* [q]
                (fresh [c t d]
                  (nom/fresh [x]
                    (typingo-deriv ['typingo c () `((~'fn ~(nom/tie x x)) (~'fn ~(nom/tie x x))) t] d)
                    (== q d))))))))))

)
