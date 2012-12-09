;; Inspired by
;;
;; William E. Byrd, Eric Holk, and Daniel P. Friedman.
;; miniKanren, Live and Untagged: Quine Generation via Relational Interpreters (Programming Pearl).
;;
;; http://www.infoq.com/presentations/miniKanren
;; https://github.com/webyrd/quines
;; http://users-cs.au.dk/danvy/sfp12/papers/byrd-holk-friedman-paper-sfp12.pdf

(ns tapl.quines
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:use [clojure.core.match :only [match]]))

(defn not-in-envo [x env]
  (conde
   [(fresh (y v rest)
           (conso [y v] rest env)
           (!= y x)
           (not-in-envo x rest))]
   [(== '() env)]))

(defn symbolo [x]
  (predc x symbol? `symbol?))

(defn progo? [x]
  (not (some #{:closure} (flatten (list x)))))

(defn progo [x]
  (predc x progo? `progo?))

(defn lookupo [x env t]
  (fresh (y v rest)
         (conso [y v] rest env)
         (conde
          ((== y x) (== v t))
          ((!= y x) (lookupo x rest t)))))

(declare proper-listo)

(defn eval-expo [exp env val]
  (conde
   [(fresh (v)
           (== `(~'quote ~v) exp)
           (not-in-envo 'quote env)
           (progo v)
           (== v val))]
   [(fresh (as)
           (conso 'list as exp)
           (not-in-envo 'list env)
           (progo as)
           (proper-listo as env val))]
   [(symbolo exp) (lookupo exp env val)]
   [(fresh (rator rand x body envc a)
           (== `(~rator ~rand) exp)
           (eval-expo rator env [:closure x body envc])
           (eval-expo rand env a)
           (eval-expo body (lcons [x a] envc) val))]
   [(fresh (x body)
           (== `(~'fn [~x] ~body) exp)
           (symbolo x)
           (not-in-envo 'fn env)
           (== [:closure x body env] val))]))

(defn proper-listo [exp env val]
  (conde
   [(== '() exp)
    (== '() val)]
   [(fresh (a d t-a t-d)
           (conso a d exp)
           (conso t-a t-d val)
           (eval-expo a env t-a)
           (proper-listo d env t-d))]))
