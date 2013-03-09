(ns tapl.letpoly
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn nomo [x]
  (predc x nom? `nom?))

(defn env-lookupo [x env t]
  (fresh [a as v vs]
    (== [(lcons a as) (lcons v vs)] env)
    (conde
      [(== a x) (== v t)]
      [(!= a x) (env-lookupo x [as vs] t)])))

(defn env-bindo [x t envi envo]
  (fresh [as vs]
    (== [as vs] envi)
    (== [(lcons x as) (lcons t vs)] envo)))

(def env-empty [() ()])

(defn inst-loopo [envi ti to envo]
  (fresh [as vs]
    (== [as vs] envi)
    (conda
      [(lvaro ti) (== ti to) (== envi envo)]
      [(conde
         [(fresh [a]
            (nomo a)
            (== ['QVar a] ti)
            (conde
              [(nom/hash a as)
               (env-bindo a to envi envo)]
              [(env-lookupo a envi to)]))]
         [(fresh [ti1 ti2 to1 to2 env1]
            (== ['TArrow ti1 ti2] ti)
            (== ['TArrow to1 to2] to)
            (inst-loopo envi ti1 to1 env1)
            (inst-loopo env1 ti2 to2 envo))])])))

(defn insto [ti to]
  (fresh [envo]
    (inst-loopo env-empty ti to envo)))

(defn gen [env t]
  (fresh [as vs]
    (== [as vs] env)
    (conda
      [(all
         (lvaro t)
         (nom/fresh [a]
           (== ['QVar a] t)
           (nom/hash a vs)))]
      [(lvaro t)]
      [(conde
         [(fresh [t1 t2]
            (== ['TArrow t1 t2] t)
            (gen env t1)
            (gen env t2))]
         [(fresh [a]
            (nomo a)
            (== ['QVar a] t))])])))

(defn typeofo [env e t]
  (fresh [as vs]
    (== [as vs] env)
    (conde
      [(fresh [x s]
         (== ['var x] e)
         (env-lookupo x env s)
         (insto s t))]
      [(fresh [e0 t0 env0 tc]
         (nom/fresh [c]
           (== ['lam (nom/tie c e0)] e)
           (== ['TArrow tc t0] t)
           (nom/hash c as)
           (env-bindo c tc env env0)
           (typeofo env0 e0 t0)))]
      [(fresh [e1 e2 t1 t2]
         (== ['app e1 e2] e)
         (== ['TArrow t2 t] t1)
         (typeofo env e1 t1)
         (typeofo env e2 t2))]
      [(fresh [ec tc e0 env0]
         (nom/fresh [c]
           (== ['let ec (nom/tie c e0)] e)
           (nom/hash c as)
           (typeofo env ec tc)
           (gen env tc)
           (env-bindo c tc env env0)
           (typeofo env0 e0 t)))])))
