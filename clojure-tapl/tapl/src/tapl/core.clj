(ns tapl.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:use [clojure.core.match :only [match]]))

; 3.2.1  Terms, inductively  (p. 26)
(defn T? [t]
  (match t
    :true true
    :false true
    :zero true
    [:succ t1] (T? t1)
    [:pred t1] (T? t1)
    [:if t1 t2 t3] (and (T? t1) (T? t2) (T? t3))
    :else false))

; 3.2.2  Terms, by inference rules (p. 26)
(defn T [t]
  (conde
    [(== :true t)]
    [(== :false t)]
    [(== :zero t)]
    [(fresh [t1]
       (conde
         [(== [:succ t1] t)]
         [(== [:pred t1] t)]
         [(== [:iszero t1] t)])
       (T t1))]
   [(fresh [t1 t2 t3]
      (== [:if t1 t2 t3] t)
      (T t1)
      (T t2)
      (T t3))]))

;; 3.2.3 Terms, concretely, with i a Peano number
(defn add-for-eacho [fo ts sa sb]
  (conde
   [(== ts []) (== sa sb)]
   [(fresh [h ho t si]
      (conso h t ts)
      (fo h ho)
      (conso ho si sa)
      (add-for-eacho fo t si sb))]))
(defn prependo [ha b out1 out2]
  (conde
   [(== b []) (== out1 out2)]
   [(fresh [hb tb oi]
      (conso hb tb b)
      (conso [ha hb] oi out1)
      (prependo ha tb oi out2))]))
(defn prodo [a b out1 out2]
  (conde
   [(== a []) (== out1 out2)]
   [(fresh [h t o1 o2 o3]
      (conso h t a)
      (prependo h b o1 o2)
      (prodo t b o2 o3)
      (== out1 o1)
      (== out2 o3))]))
(defn S [i ts]
  (conde
   [(== i :z) (== ts [])]
   [(== i [:s :z]) (== ts [:true :false :zero])]
   [(fresh (i1 i2 ts1 ts12 ts13 sa sb sc sd se sf sg sh)
      (== i [:s i1])
      (== i1 [:s i2])
      (S i1 ts1)
      (add-for-eacho (fn [x o] (== o :true)) [1] sa sb)
      (add-for-eacho (fn [x o] (== o :false)) [1] sb sc)
      (add-for-eacho (fn [x o] (== o :zero)) [1] sc sd)
      (add-for-eacho (fn [x o] (== o [:succ x])) ts1 sd se)
      (add-for-eacho (fn [x o] (== o [:pred x])) ts1 se sf)
      (add-for-eacho (fn [x o] (== o [:iszero x])) ts1 sf sg)
      (prodo ts1 ts1 ts12 [])
      (prodo ts1 ts12 ts13 [])
      (add-for-eacho (fn [x o] (fresh [x1 x2 x3]
                                 (== [x1 [x2 x3]] x)
                                 (== o [:if x1 x2 x3]))) ts13 sg sh)
      (== ts sa)
      (== sh []))]))

; 3.2 Arithmetic expressions (NB) (pp. 34 & 41)
(defn NV [t]
  (conde
   [(== :zero t)]
   [(fresh [nv]
           (== [:succ nv] t)
           (NV nv))]))

(defn V [t]
  (conde
   [(== :true t)]
   [(== :false t)]
   [(NV t)]))

(defn E [t tr]
  (conde
   [(fresh [t2 t3] ;E-IfTrue
           (== [:if :true t2 t3] t)
           (== t2 tr))]
   [(fresh [t2 t3] ;E-IfFalse
           (== [:if :false t2 t3] t)
           (== t3 tr))]
   [(fresh [t1 t2 t3 t1r] ;E-If
           (== [:if t1 t2 t3] t)
           (== [:if t1r t2 t3] tr)
           (E t1 t1r))]
   [(fresh [t1 t1r] ;E-Succ
           (== [:succ t1] t)
           (== [:succ t1r] tr)
           (E t1 t1r))]
   [(== [:pred :zero] t) ;E-PredZero
    (== :zero tr)]
   [(fresh [nv1] ;E-PredSucc
           (== [:pred [:succ nv1]] t)
           (== nv1 tr)
           (NV nv1))]
   [(fresh [t1 t1r] ; E-Pred
           (== [:pred t1] t)
           (== [:pred t1r] tr)
           (E t1 t1r))]
   [(== [:iszero :zero] t) ;E-IszeroZero
    (== :true tr)]
   [(fresh [nv1] ;E-IszeroSucc
           (== [:iszero [:succ nv1]] t)
           (== :false tr)
           (NV nv1))]
   [(fresh [t1 t1r] ;E-IsZero
           (== [:iszero t1] t)
           (== [:iszero t1r] tr)
           (E t1 t1r))]))

; 8.2 Typed Arithmetic Expressions (p. 93)
(defn TY [ty]
  (conde
   [(== :Bool ty)]
   [(== :Nat ty)]))

(defn TC [t ty]
  (conde
   [(== :true t) ;T-True
    (== :Bool ty)]
   [(== :false t) ;T-False
    (== :Bool ty)]
   [(fresh [t1 t2 t3] ;T-If
           (== [:if t1 t2 t3] t)
           (TC t1 :Bool)
           (TC t2 ty)
           (TC t3 ty))]
   [(== :zero t) ;T-Zero
    (== :Nat ty)]
   [(fresh [t1] ;T-Succ
           (== ty :Nat)
           (== [:succ t1] t)
           (TC t1 :Nat))]
   [(fresh [t1] ;T-Pred
           (== ty :Nat)
           (== [:pred t1] t)
           (TC t1 :Nat))]
   [(fresh [t1] ;T-IsZero
           (== ty :Bool)
           (== [:iszero t1] t)
           (TC t1 :Nat))]))
