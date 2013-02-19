(ns tapl.quines_nom_red_small_debug
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
    [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [logically.art.interpreters.meta_debug])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

(defn quines-solver-clause [head tail]
  (conde
    [(fresh [x]
       (== head ['nomo x])
       (predc x nom? `nom?)
       (== tail ()))]
    [(fresh [e new a out]
       (== head ['substo e new a out])
       (conde
         [(== () e) (== e out) (== tail ())]
         [(== tail [['nomo e]]) (== a e) (== new out)]
         [(fresh [v]
            (== `(~'quote ~v) e)
            (== `(~'quote ~v) out))
           (== tail ())]
         [(== tail [['nomo e]])
          (== e out)
          (!= a e)]
         [(fresh [body bodyres]
            (nom/fresh [c]
              (== `(~'fn ~(nom/tie c body)) e)
              (== `(~'fn ~(nom/tie c bodyres)) out)
              (nom/hash c a)
              (nom/hash c new)
              (== tail [['substo body new a bodyres]])))]
         [(fresh [rator ratorres rand randres]
            (== `(~rator ~rand) e)
            (== `(~ratorres ~randres) out)
            (== tail [['substo rator new a ratorres]
                      ['substo rand new a randres]]))]
         [(fresh [first rest firstres restres]
            (== `(~'cons ~first ~rest) e)
            (== `(~'cons ~firstres ~restres) out)
            (== tail [['substo first new a firstres]
                      ['substo rest new a restres]]))]))]
    [(fresh [exp]
       (== head ['valo exp])
       (== tail ())
       (conde
         [(== exp ())]
         [(fresh [v]
            (== exp `(~'quote ~v)))]
         [(fresh [body]
            (nom/fresh [x]
              (== exp `(~'fn ~(nom/tie x body)))))]))]
    [(fresh [exp v]
       (== head ['valofo exp v])
       (== tail ())
       (conde
         [(== exp ()) (== v ())]
         [(== exp `(~'quote ~v))]
         [(fresh [body]
            (nom/fresh [x]
              (== v exp)
              (== exp `(~'fn ~(nom/tie x body)))))]))]
    [(fresh [exp out]
       (== head ['redo exp out])
       (conde
         [(fresh [first firstres rest]
            (== `(~'cons ~first ~rest) exp)
            (== `(~'cons ~firstres ~rest) out)
            (== tail [['redo first firstres]]))]
         [(fresh [first rest restres]
            (== `(~'cons ~first ~rest) exp)
            (== `(~'cons ~first ~restres) out)
            (== tail [['valo first]
                      ['redo rest restres]]))]
          [(fresh [first firstv rest restv v]
             (== `(~'cons ~first ~rest) exp)
             (== `(~'quote ~v) out)
             (== tail [['valo first]
                       ['valo rest]
                       ['valofo first firstv]
                       ['valofo rest restv]])
             (conso firstv restv v)
             (seqc restv))]
          [(fresh [rator rand ratorval]
             (nom/fresh [x]
               (== `(~rator ~rand) exp)
               (== `(~ratorval ~rand) out)
               (== tail [['redo rator ratorval]])))]
          [(fresh [rator rand randval]
             (nom/fresh [x]
               (== `(~rator ~rand) exp)
               (== `(~rator ~randval) out)
               (== tail [['valo rator]
                         ['redo rand randval]])))]
          [(fresh [rator rand body]
             (nom/fresh [a]
               (== `(~rator ~rand) exp)
               (== rator `(~'fn ~(nom/tie a body)))
               (== tail [['valo rand]
                         ['substo body rand a out]])))]))]
    [(fresh [e1 e2]
       (== head ['redo* e1 e2])
       (conde
         [(== tail [['valo e1] ['valofo e1 e2]])]
         [(fresh [ep]
            (== tail [['redo e1 ep]
                      ['redo* ep e2]]))]))]
    [(fresh [e1 e2]
       (== head ['qredo* e1 e2])
       (conde
         [(== tail [['redo e1 e2]])]
         [(fresh [ep]
            (== tail [['redo e1 ep]
                      ['qredo* ep e2]]))]))]))

(def quines-debug-so (debug-so-solve-for quines-solver-clause))
