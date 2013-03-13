(ns tapl.quines_sk
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(def unl-hello
  "```````````.H.e.l.l.o. .w.o.r.l.di"
)

(def unl-quine
  "`.i```s``sv``si`k``siv``sv``si`k``si``s`k..``s`k.k``s`k.```s`k.i``s`k.s``s`k.```s`k.```s`k.s``s`k.```s`k.`v``s``si`k.```s``si`k..``s``si`k.i``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.```s``si`k.s``s``si`k.v``s``si`k.```s``si`k.```s``si`k.s``s``si`k.i``s``si`k.```s``si`k.k``s``si`k.```s``si`k.```s``si`k.s``s``si`k.i``s``si`k.v``s``si`k.```s``si`k.```s``si`k.s``s``si`k.v``s``si`k.```s``si`k.```s``si`k.s``s``si`k.i``s``si`k.```s``si`k.k``s``si`k.```s``si`k.```s``si`k.s``s``si`k.i``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k..``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.k``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.i``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.s``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.s``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.vi"
)

(defn unl2sk [x] (map (fn [c] (if (= c \`) '_ (symbol (str c)))) x))
(defn sk2unl [x] (apply str (map (fn [s] (if (= s '_) "`" (str s))) x)))

(def sk-quine (unl2sk unl-quine))
(def sk-hello (unl2sk unl-hello))

(defn appo [a1 a2 b1 b2 c1 c2]
  (all
    (== a2 b1)
    (== a1 c1)
    (== b2 c2)))

(defn evalo [in1 in2 out1 out2]
  (fresh [a d]
    (conso a d in1)
    (conde
      [(== a '.)
        (fresh [c r rout1 rout2]
          (conso c r d)
          (appo (lcons c rout1) rout1 rout1 rout2 out1 out2)
          (evalo (lcons 'i r) in2 rout1 rout2))]
      [(== a '_)
        (fresh [f rf arg rarg outf]
          (evalo d (lcons f rf) out1 outf)
          (evalo rf (lcons arg rarg) outf out2)
          (conde
            [(== f 'i) (== (lcons arg rarg) in2)]))]
      [(== a 'i) (== in1 in2) (== out1 out2)])))
