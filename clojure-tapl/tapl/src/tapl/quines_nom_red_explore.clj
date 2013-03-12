(ns tapl.quines_nom_red_explore
  (:use [tapl.quines_nom_red_big_base] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]
        [clojure.core.logic.protocols]))

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
    [(fresh [rator rand body randval ratorstep randstep]
       (nom/fresh [a]
         (== `(~rator ~rand) exp)
         (!= rator 'quote)
         (!= rator 'list)
         (redfo rator ratorstep (nom/tie a body))
         (redfo rand randstep randval)
         (esubsto body randval a val)
         (conde
           [(== ratorstep 'done) (== randstep 'done) (substo body rand a step)]
           [(== ratorstep 'done) (!= randstep 'done) (== step `(~rator ~randstep))]
           [(!= ratorstep 'done) (== step `(~ratorstep ~rand))])))]))

(defn redfo* [e1 e2]
  (fresh [step]
    (redfo e1 step e2)
    (conde
      [(== step 'done)]
      [(!= step 'done)
       (fn [a] (println (-reify a step)) a)
       (redfo* step e2)])))

(comment
  (run 1 [q] (fresh [p] (nom/fresh [a] (redfo* (nom/tie a a) p) (cljo p q))))
  (run 1 [q] (fresh [p1 p2] (nom/fresh [a] (redfo* p1 p2) (cljo p1 q))))

  (run 1 [q] (fresh [p] (redfo* `(~'list (~'quote ~'a) (~'list (~'quote ~'b) (~'quote ~'c))) p) (cljo p q)))
  
  (run 1 [q] (fresh [p] (redfo* p `(~'quote ~p)) (cljo p q)))
  (((fn [a_0] (list a_0 (list (quote quote) a_0))) (quote (fn [a_1] (list a_1 (list (quote quote) a_1))))))

  (run 1 [q] (fresh [p s] (redfo p s `(~'quote ~p)) (== q s)))
  ((list (quote [a_0] (list a_0 (list (quote quote) a_0))) (list (quote quote) (quote [a_0] (list a_0 (list (quote quote) a_0)))))))

































