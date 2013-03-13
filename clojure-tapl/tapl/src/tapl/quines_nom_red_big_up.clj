(ns tapl.quines_nom_red_big_up
  (:use [tapl.utils])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [clojure.core.logic.protocols])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

(declare list-substo)

(defn substo [e new a out]
  (conde
    [(== a e) (== new out)]
    [(nomo e) (== e out) (!= a e)]
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
    [(fresh [es vs]
       (conso 'list es e)
       (conso 'list vs out)
       (list-substo es new a vs))]
    [(fresh [rator ratorres rand randres]
       (== `(~rator ~rand) e)
       (!= rator 'quote)
       (!= rator 'fn)
       (!= rator 'list)
       (== `(~ratorres ~randres) out)
       (substo rator new a ratorres)
       (substo rand new a randres))]))

(defn list-substo [es new a out]
  (conde
    [(== es ()) (== out ())]
    [(fresh [first rest firstv restv]
       (conso first rest es)
       (conso firstv restv out)
       (substo first new a firstv)
       (list-substo rest new a restv))]))

(declare evalo)
(declare list-esubsto)

(defn esubsto [e new a out]
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
    [(fresh [es vs]
       (conso 'list es e)
       (== `(~'quote ~vs) out)
       (list-esubsto es new a vs))]
    [(fresh [rator ratorres rand randres i]
       (== `(~rator ~rand) e)
       (!= rator 'quote)
       (!= rator 'fn)
       (!= rator 'list)
       (== `(~ratorres ~randres) i)
       (substo rator new a ratorres)
       (substo rand new a randres)
       (evalo i out))]))

(defn list-esubsto [es new a vs]
  (conde
    [(== es ()) (== vs ())]
    [(fresh [first rest firstv restv]
       (conso first rest es)
       (conso firstv restv vs)
       (esubsto first new a `(~'quote ~firstv))
       (list-esubsto rest new a restv))]))

(declare check-ties-o)

(defn check-ties [xp]
  (fixc xp
    ;; constraint body
    (fn [[x p] s reifier]
      (let [x (walk s x)
            p (walk s p)]
        (cond
          (tie? x) (composeg*
                     (== p `(~'fn ~x))
                     (check-ties-o (:body x)))
          (not (tree-term? x)) succeed
          :else (constrain-tree x
                  (fn [t s] ((check-ties [t x]) s))))))
    ;; runnable predicate
    (fn [[x p] a]
      (not (lvar? (walk a x))))
    ;; reifier
    (fn [_ _ _ r _]
      (let [[x p] (walk* r xp)]
        `(~'check-ties ~x ~p)))))

(defn check-ties-o [x]
  (check-ties [x nil]))

(declare list-evalo)

(defn evalo [exp val]
  (conde
    [(fresh [v]
       (== `(~'quote ~v) exp)
       (check-ties-o v)
       (== `(~'quote ~v) val))]
    [(fresh [body]
       (nom/fresh [a]
         (== `(~'fn ~(nom/tie a body)) exp)
         (== exp val)))]
    [(fresh [es vs]
       (conso 'list es exp)
       (== `(~'quote ~vs) val)
       (list-evalo es vs))]
    [(fresh [rator rand body randval]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (!= rator 'quote)
         (!= rator 'fn)
         (!= rator 'list)
         (evalo rator `(~'fn ~(nom/tie a body)))
         (evalo rand randval)
         (esubsto body randval a val)))]))

(defn list-evalo [es vs]
  (conde
    [(== es ()) (== vs ())]
    [(fresh [first rest firstv restv]
       (conso first rest es)
       (conso firstv restv vs)
       (evalo first `(~'quote ~firstv))
       (list-evalo rest restv))]))
