(ns tapl.quines_nom_red_explore
  (:use [tapl.quines_nom_red_big_base])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [clojure.core.logic.protocols]))

(declare redfo)

(defn substfo [e new a out val]
  (conde
    [(== a e) (== new out) (== out val)]
    [(fresh [v]
       (== `(~'quote ~v) e)
       (== `(~'quote ~v) out)
       (== out val))]
    [(fresh [body bodyres]
       (nom/fresh [c]
         (== (nom/tie c body) e)
         (== (nom/tie c bodyres) out)
         (== out val)
         (nom/hash c a)
         (nom/hash c new)
         (substo body new a bodyres)))]
    [(fresh [e1 e2 o1 o2 v1 v2]
       (== `(~'list ~e1 ~e2) e)
       (== `(~'list ~o1 ~o2) out)
       (== `(~'quote (~v1 ~v2)) val)
       (substfo e1 new a o1 `(~'quote ~v1))
       (substfo e2 new a o2 `(~'quote ~v2)))]
    [(fresh [rator rand ratoro rando ratorval randval ignore-step]
       (== `(~rator ~rand) e)
       (== `(~ratoro ~rando) out)
       (!= rator 'quote)
       (!= rator 'list)
       (substfo rator new a ratoro ratorval)
       (substfo rand new a rando randval)
       (redfo `(~ratorval ~randval) ignore-step val))]))

(defn redfo [exp step val]
  (conde
    [(fresh [v]
       (== `(~'quote ~v) exp)
       (== `(~'quote ~v) val)
       (== step 'done))]
    [(fresh [body]
       (nom/fresh [a]
         (== (nom/tie a body) exp)
         (== exp val)
         (== step 'done)))]
    [(fresh [es vs]
       (conso 'list es exp)
       (== `(~'quote ~vs) val)
       (fresh [e1 e2 s1 s2 v1 v2]
         (== vs `(~v1 ~v2))
         (== `(~'list ~e1 ~e2) exp)
         (conde
           [(== s1 'done) (== s2 'done) (== step val)]
           [(== s1 'done) (!= s2 'done) (== step `(~'list ~e1 ~s2))]
           [(!= s1 'done) (== step `(~'list ~s1 ~e2))])
         (redfo e1 s1 `(~'quote ~v1))
         (redfo e2 s2 `(~'quote ~v2))))]
    [(fresh [rator rand body randval ratorstep randstep subst-step]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (!= rator 'quote)
         (!= rator 'list)
         (conde
           [(== ratorstep 'done) (== randstep 'done) (== step subst-step)]
           [(== ratorstep 'done) (!= randstep 'done) (== step `(~rator ~randstep))]
           [(!= ratorstep 'done) (== step `(~ratorstep ~rand))])
         (redfo rator ratorstep (nom/tie a body))
         (redfo rand randstep randval)
         (substfo body randval a subst-step val)))]))

(defn redfo* [e1 e2]
  (fresh [step]
    (redfo e1 step e2)
    (conde
      [(== step 'done)]
      [(!= step 'done)
       ;(fn [a] (println (-reify a step)) a)
       (redfo* step e2)])))

