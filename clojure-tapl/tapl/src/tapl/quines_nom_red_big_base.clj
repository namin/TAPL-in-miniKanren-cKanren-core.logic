(ns tapl.quines_nom_red_big_base
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [clojure.core.logic.protocols]))

(defn nomo [x]
  (predc x nom? `nom?))
(defn symbolo [x]
  (predc x symbol? `symbol?))

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
         (== (nom/tie c body) e)
         (== (nom/tie c bodyres) out)
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
         (== (nom/tie c body) e)
         (== (nom/tie c bodyres) out)
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

(declare list-evalo)

(defn evalo [exp val]
  (conde
    [(fresh [v]
       (== `(~'quote ~v) exp)
       (== `(~'quote ~v) val))]
    [(fresh [body]
       (nom/fresh [a]
         (== (nom/tie a body) exp)
         (== exp val)))]
    [(fresh [es vs]
       (conso 'list es exp)
       (== `(~'quote ~vs) val)
       (list-evalo es vs))]
    [(fresh [rator rand body randval]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (!= rator 'quote)
         (!= rator 'list)
         (evalo rator (nom/tie a body))
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

(defn cljo [e out]
  (conda
    [(lvaro e) (nom/fresh [x] (== out `(~'quote ~x)))]
    [(nomo e) (== e out)]
    [(symbolo e) (== e out)]
    [(== e ()) (== e out)]
    [(fresh [body bodyr]
       (nom/fresh [x]
         (== e (nom/tie x body))
         (== out `(~'fn [~x] ~bodyr))
         (cljo body bodyr)))]
    [(fresh [a d ar dr]
       (conso a d e)
       (conso ar dr out)
       (cljo a ar)
       (cljo d dr))]))
