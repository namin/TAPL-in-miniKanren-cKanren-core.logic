(ns tapl.quines_nom_red_big_up_shadow
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [clojure.core.logic.protocols])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

(defn nomo [x]
  (predc x nom? `nom?))

(declare list-substo)

(defn substo [noms e new a out]
  (fresh [fn-nom list-nom quote-nom]
    (== noms [fn-nom list-nom quote-nom])
    (conde
      [(== a e) (== new out)]
      [(nomo e) (== e out) (!= a e)]
      [(fresh [v]
         (== `(~quote-nom ~v) e)
         (== `(~quote-nom ~v) out))]
      [(fresh [body bodyres]
         (nom/fresh [c]
           (== `(~fn-nom ~(nom/tie c body)) e)
           (== `(~fn-nom ~(nom/tie c bodyres)) out)
           (nom/hash c noms)
           (nom/hash c a)
           (nom/hash c new)
           (substo noms body new a bodyres)))]
      [(fresh [es vs]
         (conso list-nom es e)
         (conso list-nom vs out)
         (list-substo noms es new a vs))]
      [(fresh [rator ratorres rand randres]
         (== `(~rator ~rand) e)
         (!= rator quote-nom)
         (!= rator fn-nom)
         (!= rator list-nom)
         (== `(~ratorres ~randres) out)
         (substo noms rator new a ratorres)
         (substo noms rand new a randres))])))

(defn list-substo [noms es new a out]
  (conde
    [(== es ()) (== out ())]
    [(fresh [first rest firstv restv]
       (conso first rest es)
       (conso firstv restv out)
       (substo noms first new a firstv)
       (list-substo noms rest new a restv))]))

(declare evalo)
(declare list-esubsto)

(defn esubsto [noms e new a out]
  (fresh [fn-nom list-nom quote-nom]
    (== noms [fn-nom list-nom quote-nom])
    (conde
      [(== a e) (== new out)]
      [(fresh [v]
         (== `(~quote-nom ~v) e)
         (== `(~quote-nom ~v) out))]
      [(fresh [body bodyres]
         (nom/fresh [c]
           (== `(~fn-nom ~(nom/tie c body)) e)
           (== `(~fn-nom ~(nom/tie c bodyres)) out)
           (nom/hash c noms)
           (nom/hash c a)
           (nom/hash c new)
           (substo noms body new a bodyres)))]
      [(fresh [es vs]
         (conso list-nom es e)
         (== `(~quote-nom ~vs) out)
         (list-esubsto noms es new a vs))]
      [(fresh [rator ratorres rand randres i]
         (== `(~rator ~rand) e)
         (!= rator quote-nom)
         (!= rator fn-nom)
         (!= rator list-nom)
         (== `(~ratorres ~randres) i)
         (substo noms rator new a ratorres)
         (substo noms rand new a randres)
         (evalo noms i out))])))

(defn list-esubsto [noms es new a vs]
  (fresh [fn-nom list-nom quote-nom]
    (== noms [fn-nom list-nom quote-nom])
    (conde
      [(== es ()) (== vs ())]
      [(fresh [first rest firstv restv]
         (conso first rest es)
         (conso firstv restv vs)
         (esubsto noms first new a `(~quote-nom ~firstv))
         (list-esubsto noms rest new a restv))])))

(declare check-ties-o)

(defn check-ties [fxp]
  (fixc fxp
    ;; constraint body
    (fn [[fn-nom x p] s reifier]
      (let [fn-nom (walk s fn-nom)
            x (walk s x)
            p (walk s p)]
        (cond
          (tie? x) (composeg*
                     (== p `(~fn-nom ~x))
                     (check-ties-o fn-nom (:body x)))
          (not (tree-term? x)) succeed
          :else (constrain-tree x
                  (fn [t s] ((check-ties [fn-nom t x]) s))))))
    ;; runnable predicate
    (fn [[fn-nom x p] a]
      (not (lvar? (walk a x))))
    ;; reifier
    (fn [_ _ _ r _]
      (let [[fn-nom x p] (walk* r fxp)]
        `(~'check-ties ~x ~p)))))

(defn check-ties-o [fn-nom x]
  (check-ties [fn-nom x nil]))

(declare list-evalo)

(defn evalo [noms exp val]
  (fresh [fn-nom list-nom quote-nom]
    (== noms [fn-nom list-nom quote-nom])
    (conde
      [(fresh [v]
         (== `(~quote-nom ~v) exp)
         (check-ties-o fn-nom v)
         (== `(~quote-nom ~v) val))]
      [(fresh [body]
         (nom/fresh [a]
           (== `(~fn-nom ~(nom/tie a body)) exp)
           (== exp val)))]
      [(fresh [es vs]
         (conso list-nom es exp)
         (== `(~quote-nom vs) val)
         (list-evalo noms es vs))]
      [(fresh [rator rand body randval]
         (nom/fresh [a]
           (== `(~rator ~rand) exp)
           (!= rator quote-nom)
           (!= rator fn-nom)
           (!= rator list-nom)
           (evalo noms rator `(~fn-nom ~(nom/tie a body)))
           (evalo noms rand randval)
           (esubsto noms body randval a val)))])))

(defn list-evalo [noms es vs]
  (fresh [fn-nom list-nom quote-nom]
    (== noms [fn-nom list-nom quote-nom])
    (conde
      [(== es ()) (== vs ())]
      [(fresh [first rest firstv restv]
         (conso first rest es)
         (conso firstv restv vs)
         (evalo noms first `(~quote-nom ~firstv))
         (list-evalo noms rest restv))])))

(defn clojurifyo [noms in out]
  (fresh [fn-nom  list-nom quote-nom]
    (== noms [fn-nom list-nom quote-nom])
    (conde
      [(== in ()) (== in out)]
      [(== fn-nom in) (== 'fn out)]
      [(== quote-nom in) (== 'quote out)]
      [(== list-nom in) (== 'list out)]
      [(fresh [body bodyres]
         (nom/fresh [a]
           (== (nom/tie a body) in)
           (nom/hash a noms)
           (clojurifyo noms body bodyres)
           (== (nom/tie a bodyres) out)))]
      [(nomo in) (== in out)
       (!= in fn-nom)
       (!= in quote-nom)
       (!= in list-nom)]
      [(seqc in)
       (fresh [first rest firstres restres]
         (conso first rest in)
         (conso firstres restres out)
         (clojurifyo noms first firstres)
         (clojurifyo noms rest restres))])))
