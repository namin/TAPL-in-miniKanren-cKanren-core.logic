(ns tapl.quines_nom_red_explore
  (:use [tapl.quines_nom_red_big_base] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [clojure.core.logic.protocols]))

(defn valo [exp]
  (conde
    [(fresh [v]
       (== `(~'quote ~v) exp))]
    [(fresh [body]
       (nom/fresh [a]
         (== (nom/tie a body) exp)))]))

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
       (list-evalo es vs)
       (fresh [e1 e2]
         (== `(~'list ~e1 ~e2) exp)
         (conde
           [(valo e1) (valo e2) (== step val)]
           [(valo e1) (fresh [s2 v2] (!= s2 'done) (redfo e2 s2 v2) (== step `(~'list ~e1 ~s2)))]
           [(fresh [s1 v1] (!= s1 'done) (redfo e1 s1 v1) (== step `(~'list ~s1 ~e2)))])))]
    [(fresh [rator rand body randval]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (!= rator 'quote)
         (!= rator 'list)
         (evalo rator (nom/tie a body))
         (evalo rand randval)
         (esubsto body randval a val)
         (conde
           [(valo rator) (valo rand) (substo body rand a step)]
           [(fresh [s v] (!= s 'done) (redfo rator s v) (== step `(~s ~rand)))]
           [(valo rator) (fresh [s v] (!= s 'done) (redfo rand s v) (== step `(~rator ~s)))])))]))

(defn redfo* [e1 e2]
  (fresh [step]
    (redfo e1 step e2)
    (conde
      [(== step 'done)]
      [(!= step 'done)
       (redfo* step e2)])))

(comment
  (run 1 [q] (fresh [p] (nom/fresh [a] (redfo* (nom/tie a a) p) (cljo p q))))
  (run 1 [q] (fresh [p1 p2] (nom/fresh [a] (redfo* p1 p2) (cljo p1 q))))

  (run 1 [q] (fresh [p] (redfo* `(~'list (~'quote ~'a) (~'list (~'quote ~'b) (~'quote ~'c))) p) (cljo p q)))
  
  (run 1 [q] (fresh [p] (redfo* p `(~'quote ~p)) (cljo p q)))
  (((fn [a_0] (list a_0 (list (quote quote) a_0))) (quote (fn [a_1] (list a_1 (list (quote quote) a_1))))))

  (run 1 [q] (fresh [p s] (redfo p s `(~'quote ~p)) (== q s)))
  ((list (quote [a_0] (list a_0 (list (quote quote) a_0))) (list (quote quote) (quote [a_0] (list a_0 (list (quote quote) a_0)))))))

