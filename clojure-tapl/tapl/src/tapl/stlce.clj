(ns tapl.stlce
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:require [clojure.pprint :as pp]))

;; Pure simply typed lambda-calculus with type reconstruction
;; and error messages

(defn nomo [x]
  (predc x nom? `nom?))

(defn termo [t]
  (conde
    ;; variable
    [(nomo t)]
    ;; abstraction
    [(fresh [t1]
       (nom/fresh [x]
         (== t `(~'fn ~(nom/tie x t1)))
         (termo t1)))]
    ;; application
    [(fresh [t1 t2]
       (== t `(~t1 ~t2))
       (termo t1)
       (termo t2))]))

(defn valo [t]
  ;; abstraction
  (fresh [t1]
    (nom/fresh [x]
      (== t `(~'fn ~(nom/tie x t1)))
      (termo t1))))

(defn substo [x v t out]
  (conde
    [(== x t) (== v out)]
    [(nomo t) (== t out) (nom/hash x t)]
    [(fresh [t1 t1r]
       (nom/fresh [y]
         (== t `(~'fn ~(nom/tie y t1)))
         (== t `(~'fn ~(nom/tie y t1r)))
         (nom/hash y x)
         (nom/hash y v)
         (substo x v t1 t1r)))]
    [(fresh [t1 t2 t1r t2r]
       (== t `(~t1 ~t2))
       (== out `(~t1r ~t2r))
       (substo x v t1 t1r)
       (substo x v t2 t2r))]))

(defn redo [t tp]
  (conde
    ;; E-App1
    [(fresh [t1 t2 t1p]
       (== t `(~t1 ~t2))
       (== tp `(~t1p ~t2))
       (termo t1)
       (termo t2)
       (redo t1 t1p))]
    ;; E-App2
    [(fresh [v1 t2 t2p]
       (== t `(~v1 ~t2))
       (== tp `(~v1 ~t2p))
       (valo v1)
       (termo t2)
       (redo t2 t2p))]
    ;; E-AppAbs
    [(fresh [t12 v2]
       (nom/fresh [x]
         (== t `((~'fn ~(nom/tie x t12)) ~v2))
         (valo v2)
         (termo t12)
         (substo x v2 t12 tp)))]))

(defn redo* [t out]
  (conde
    [(valo t) (== t out)]
    [(fresh [tp]
       (redo t tp)
       (redo* tp out))]))

(defn lookupo [gamma x tyb]
  (conde
    [(== () gamma) (== tyb `(~'error (~'unbound-var)))]
    [(fresh [gammap y tyy]
       (conso [y tyy] gammap gamma)
       (conde
         [(== y x) (== tyb `(~'type ~tyy))]
         ;; TODO(namin): we get spurious results if we use nom/hash instead of !=.
         ;;  This indicates a potential problem with nom/hash and variable noms.
         [(!= y x) (lookupo gammap x tyb)]))]))

(defn typingo [gamma t tyb]
  (conde
    ;; T-Var
    [(nomo t) (lookupo gamma t tyb)]
    ;; T-Abs
    [(fresh [gammax ty1 ty2 t2 ty tyb2 msg]
       (nom/fresh [x]
         (== t `(~'fn ~(nom/tie x t2)))
         (== ty [:=> ty1 ty2])
         (conso [x ty1] gamma gammax)
         (typingo gammax t2 tyb2)
         (conde
           [(== tyb2 `(~'type ~ty2))
            (== tyb `(~'type ~ty))]
           [(== tyb2 `(~'error ~msg))
            (== tyb tyb2)])))]
    ;; T-App
    [(fresh [ty11 ty12 t1 t2 tyb2 tyb1 ty1 ty2 msg]
       (== t `(~t1 ~t2))
       (!= t1 'fn)
       (typingo gamma t1 tyb1)
       (typingo gamma t2 tyb2)
       (conde
         [(== tyb1 `(~'type ~ty1))
          (== tyb2 `(~'type ~ty2))
           (conde
             [(!= ty1 [:=> ty11 ty12])
              (== tyb `(~'error (~'not-arrow-type ~ty1)))]
             [(== ty1 [:=> ty11 ty12])
               (conde
                 [(!= ty11 ty2)
                   (== tyb `(~'error (~'incompatible-types ~ty11 ~ty2)))]
                 [(== ty11 ty2)
                  (== tyb `(~'type ~ty12))])])]
         [(== tyb1 `(~'error ~msg))
          (== tyb tyb1)]
         [(== tyb2 `(~'error ~msg))
          (== tyb1 `(~'type ~ty1))
           (== tyb tyb2)]))]))
