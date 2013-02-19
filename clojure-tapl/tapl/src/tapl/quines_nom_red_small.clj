(ns tapl.quines_nom_red_small
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

(defn nomo [x]
  (predc x nom? `nom?))

;;; NOTE(webyrd) The big difference between this version and
;;; quines_nom.clj seems to be the expense (that is, large branching
;;; factor) of substo.

;;; NOTE(webyrd) I suspect list is more efficient than cons.

(defn substo [e new a out]
  (conde
    [(== () e) (== e out)]
    [(nomo e) (== a e) (== new out)]
    [(fresh [v]
       (== `(~'quote ~v) e)
       (== `(~'quote ~v) out))]
    [(nomo e)
      (== e out)
      (!= a e)]
    [(fresh [body bodyres]
       (nom/fresh [c]
         (== `(~'fn ~(nom/tie c body)) e)
         (== `(~'fn ~(nom/tie c bodyres)) out)
         (nom/hash c a)
         (nom/hash c new)
         (substo body new a bodyres)))]
    [(fresh [rator ratorres rand randres]
       (== `(~rator ~rand) e)
       (== `(~ratorres ~randres) out)
       (substo rator new a ratorres)
       (substo rand new a randres))]
    [(fresh [first rest firstres restres]
       (== `(~'cons ~first ~rest) e)
       (== `(~'cons ~firstres ~restres) out)
       (substo first new a firstres)
       (substo rest new a restres))]))

;; NOTE(namin): Only handling close terms, so noms are not values.
(defn valo [exp]
  (conde
    [(== exp ())]
    [(fresh [v]
       (== exp `(~'quote ~v)))]
    [(fresh [body]
       (nom/fresh [x]
         (== exp `(~'fn ~(nom/tie x body)))))]))

(defn valofo [exp v]
  (conde
    [(== exp ()) (== v ())]
    [(== exp `(~'quote ~v))]
    [(fresh [body]
       (nom/fresh [x]
         (== v exp)
         (== exp `(~'fn ~(nom/tie x body)))))]))

(defn qvalofo [exp v]
  (conde
    [(== exp ()) (== v ())]
    [(== exp `(~'quote ~v))]))

(defn redo [exp out]
  (conde
    [(fresh [first firstres rest]
       (== `(~'cons ~first ~rest) exp)
       (== `(~'cons ~firstres ~rest) out)
       (redo first firstres))]
    [(fresh [first rest restres]
       (== `(~'cons ~first ~rest) exp)
       (== `(~'cons ~first ~restres) out)
       (valo first)
       (redo rest restres))]
    [(fresh [first firstv rest restv v]
       (== `(~'cons ~first ~rest) exp)
       (== `(~'quote ~v) out)
       (valo first)
       (valo rest)
       (qvalofo first firstv)
       (qvalofo rest restv)
       (conso firstv restv v)
       (seqc restv))]
    [(fresh [rator rand ratorval]
       (nom/fresh [x]
         (== `(~rator ~rand) exp)
         (== `(~ratorval ~rand) out)
         (redo rator ratorval)))]
    [(fresh [rator rand randval]
       (nom/fresh [x]
         (== `(~rator ~rand) exp)
         (== `(~rator ~randval) out)
         (valo rator)
         (redo rand randval)))]
    [(fresh [rator rand body]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (== rator `(~'fn ~(nom/tie a body)))
         (valo rand)
         (substo body rand a out)))]))

;; A canonical multistep reducer.
(defn redo* [e1 e2]
 (conde
   [(valo e1) (valofo e1 e2)]
   [(fresh [ep]
      (redo e1 ep)
      (redo* ep e2))]))

(def tredo*
  (tabled [e1 e2]
     (conde
       [(valo e1) (valofo e1 e2)]
       [(fresh [ep]
          (redo e1 ep)
          (tredo* ep e2))])))

;; An experimental multistep reducer which specializes in "true" quines, via (qredo* v `(~'quote ~v)).
(defn qredo* [e1 e2]
  (conde
    [(redo e1 e2)]
    [(fresh [ep]
       (redo e1 ep)
       (qredo* ep e2))]))

(defn tqredo* [e1 e2]
  (conde
    [(redo e1 e2)]
    [(fresh [ep]
       (redo e1 ep)
       (tqredo* ep e2))]))

