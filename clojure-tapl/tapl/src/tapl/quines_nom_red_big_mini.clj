(ns tapl.quines_nom_red_big_mini
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

(defn substo [e new a out]
  (conde
    [(== a e) (== new out)]
    [(fresh [v]
       (== `(~'quote ~v) e)
       (== `(~'quote ~v) out))]
    [(fresh [body bodyres]
       (nom/fresh [c]
         (== `(~'fn ~(nom/tie c body)) e)
         (== `(~'fn ~(nom/tie c bodyres)) out)
         (nom/hash c a)
         (nom/hash c new)
         (substo body new a bodyres)))]
    [(fresh [x xres y yres]
       (== `(~'list ~x ~y) e)
       (== `(~'list ~xres ~yres) out)
       (substo x new a xres)
       (substo y new a yres))]
    [(fresh [rator ratorres rand randres body]
       (== `(~rator ~rand) e)
       (nom/fresh [c]
         (== rator `(~'fn ~(nom/tie c body))))
       (== `(~ratorres ~randres) out)
       (substo rator new a ratorres)
       (substo rand new a randres))]))

(declare evalo)

(defn esubsto [e new a out]
  (conde
    [(== a e) (== new out)]
    [(fresh [v]
       (== `(~'quote ~v) e)
       (== v out))]
    [(fresh [x xres y yres]
       (== `(~'list ~x ~y) e)
       (== `(~xres ~yres) out)
       (esubsto x new a xres)
       (esubsto y new a yres))]
    [(fresh [rator ratorres rand randres body i]
       (nom/fresh [c]
         (== `(~rator ~rand) e)
         (== rator `(~'fn ~(nom/tie c body)))
         (== `(~ratorres ~randres) i)
         (substo rator new a ratorres)
         (substo rand new a randres)
         (evalo i out)))]))

(defn evalo [exp val]
  (conde
    [(fresh [v]
       (== `(~'quote ~v) exp)
       (== v val))]
    [(fresh [x xval y yval]
       (== `(~'list ~x ~y) exp)
       (== `(~xval ~yval) val)
       (evalo x xval)
       (evalo y yval))]
    [(fresh [rator rand body randval]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (== rator `(~'fn ~(nom/tie a body)))
         (evalo rand randval)
         (esubsto body randval a val)))]))
