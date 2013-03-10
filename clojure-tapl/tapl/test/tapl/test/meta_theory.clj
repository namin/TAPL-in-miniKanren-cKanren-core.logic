(ns tapl.test.meta_theory
  (:use [tapl.meta_theory] :reload)
  (:use [tapl.stlc_ext])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test])
)

(defn- typingo-bad-preservation [gamma t ty]
  (conde
    ;; T-Var
    [(nomo t) (lookupo gamma t ty)]
    ;; T-Abs
    [(fresh [gammax ty1 ty2 t2]
       (nom/fresh [x]
         (== t `(~'fn ~(nom/tie x t2)))
         (== ty [:=> ty1 ty2])
         (conso [x ty1] gamma gammax)
         (typingo gammax t2 ty2)))]
    ;; T-App
    [(fresh [ty11 ty12 t1 t2]
       (== t `(~t1 ~t2))
       (!= 'fn t1)
       (== ty ty12)
       (typingo gamma t1 [:=> ty11 ty12])
       (typingo gamma t2 ty11))]
    ;; T-True
    [(== t true) (== ty 'bool)]
    ;; T-False
    [(== t false) (== ty 'bool)]
    ;; T-If
    [(fresh [tc ta tb]
       (== t `(~'if ~tc ~ta ~tb))
       (typingo gamma tc 'bool)
       (typingo gamma ta ty)
       ;; wrong: missing goal (typingo gamma tb ty)
       )]))

(defn- typingo-bad-progress [gamma t ty]
  (conde
    ;; T-Var
    [(nomo t) (lookupo gamma t ty)]
    ;; T-Abs
    [(fresh [gammax ty1 ty2 t2]
       (nom/fresh [x]
         (== t `(~'fn ~(nom/tie x t2)))
         (== ty [:=> ty1 ty2])
         (conso [x ty1] gamma gammax)
         (typingo gammax t2 ty2)))]
    ;; T-App
    [(fresh [ty11 ty12 t1 t2]
       (== t `(~t1 ~t2))
       (!= 'fn t1)
       (== ty ty12)
       (typingo gamma t1 [:=> ty11 ty12])
       (typingo gamma t2 ty11))]
    ;; T-True
    [(== t true) (== ty 'bool)]
    ;; T-False
    [(== t false) (== ty 'bool)]
    ;; T-If
    [(fresh [tc ta tb tyc]
       (== t `(~'if ~tc ~ta ~tb))
       (typingo gamma tc tyc) ;; wrong: tyc should be 'bool
       (typingo gamma ta ty)
       (typingo gamma tb ty))]))

(def max-tries 3000)

(deftest test-stlc-ext-1
  (is (= (run 1 [q] ((check-preservation typingo redo) q max-tries))
         `((~'reached ~max-tries))))
  (is (= (run 1 [q] ((check-preservation typingo-bad-preservation redo) q max-tries))
        `([~'counterexample (~'if ~false ~true (~'fn  ~(nom/tie 'a_0 'a_0))) (~'fn  ~(nom/tie 'a_0 'a_0)) ~'bool ~'[:=> _1 _1]])))
  (is (= (run 1 [q] ((check-progress typingo redo valo) q max-tries))
        `((~'reached ~max-tries))))
  (is (= (run 1 [q] ((check-progress typingo-bad-progress redo valo) q max-tries))
        `([~'counterexample (~'if (~'fn  ~(nom/tie 'a_0 'a_0)) ~true ~true) ~'bool]))))


