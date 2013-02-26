(ns tapl.stlc_deriv_tex
  (:use [tapl.stlc]
        [tapl.stlc_deriv])
  (:use [logically.art.interpreters.meta])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

(defn typ-rule [c]
  (= (first c) 'typingo))
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
(defn proof-head-ok [d]
  (not (= (second d) 'error)))
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
      (print "}"))
    :else
    (print "{" c "}")))

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

(defn tex-stlc-typ [c]
  (print "\\typ ")
  (tex-stlc-env (typ-env c))
  (print " ")
  (tex-stlc-term (typ-term c))
  (print " ")
  (tex-stlc-type (typ-type c)))

(declare tex-stlc-proof-tree-sub)

(defn tex-stlc-proof-tree-one [d]
  (let [c (proof-head d)]
    (if (proof-head-ok d)
      (let [tail (proof-tail d)
            r (every? (fn [x] x) (map (fn [other] (tex-stlc-proof-tree-sub other)) tail))]
        (when (typ-rule c)
          (print "\\justifies ")
          (tex-stlc-typ c)
          (println "")
          (print "\\using ")
          (when (not r)
            (print "\\err{"))
          (print "\\textsc{" (typ-case c) "}")
          (when (not r)
            (print "}"))
          (println ""))
        r)
      (do
        (cond
          (= (first c) 'lookupo)
          (println "\\err{\\text{unbound variable } " (nth c 2) "}")
          (= (first c) '==)
          (do
            (println "\\err{")
            (tex-stlc-type (nth c 1))
            (print " \\neq ")
            (tex-stlc-type (nth c 2))
            (println "}"))
          (= (first c) 'typingo)
          (do
            (print "\\err{\\text{no case for } ")
            (tex-stlc-typ c)
            (println "}")))
        false))))

(defn tex-stlc-proof-tree-sub [d]
  (when (proof-head-ok d) (println "\\["))
  (let [r (tex-stlc-proof-tree-one d)]
    (when (proof-head-ok d) (println "\\]"))
    r))

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

  (spit
    "tex/ex3.tex"
    (with-out-str
      (tex-stlc-proof-tree-top
        (read-string
          (prn-str
            (first
              (run* [q]
                (fresh [c t d]
                  (nom/fresh [f g a]
                    (typingo-deriv ['typingo c  () `(~'fn ~(nom/tie f `(~'fn ~(nom/tie g `(~'fn ~(nom/tie a `(~g (~f ~a)))))))) t] d)
                    (== q d))))))))))

  (spit
    "tex/debug_ex1.tex"
    (with-out-str
      (tex-stlc-proof-tree-top
        (read-string
          (prn-str
            (first
              (run* [q]
                (fresh [c t d ok]
                  (nom/fresh [x]
                    (typingo-debug ['typingo c () `(~'fn ~(nom/tie x [x x])) t] d ok)
                    (== q d))))))))))

  (spit
    "tex/debug_ex2.tex"
    (with-out-str
      (tex-stlc-proof-tree-top
        (read-string
          (prn-str
            (first
              (run* [q]
                (fresh [c t d ok]
                  (nom/fresh [x y]
                    (typingo-debug ['typingo c () `(~'fn ~(nom/tie x y)) t] d ok)
                    (== q d))))))))))

  (spit
    "tex/debug_ex3.tex"
    (with-out-str
      (tex-stlc-proof-tree-top
        (read-string
          (prn-str
            (first
              (run* [q]
                (fresh [c t d ok]
                  (nom/fresh [x]
                    (typingo-debug ['typingo c () `(~'fn ~(nom/tie x `(~x ~x ~x))) t] d ok)
                    (== q d))))))))))
)
