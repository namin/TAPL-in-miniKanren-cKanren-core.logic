(ns tapl.quines_sk
  (:use [tapl.utils] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(def unl-hello
  "```````````.H.e.l.l.o. .w.o.r.l.di"
)

(def unl-hello-n
  "```si`k``s.H``s.e``s.l``s.l``s.o``s. ``s.w``s.o``s.r``s.l``s.d``s.!``sii``si``si``si``si``si``si``si``si`ki"
)

(def unl-quine
  "`.i```s``sv``si`k``siv``sv``si`k``si``s`k..``s`k.k``s`k.```s`k.i``s`k.s``s`k.```s`k.```s`k.s``s`k.```s`k.`v``s``si`k.```s``si`k..``s``si`k.i``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.```s``si`k.s``s``si`k.v``s``si`k.```s``si`k.```s``si`k.s``s``si`k.i``s``si`k.```s``si`k.k``s``si`k.```s``si`k.```s``si`k.s``s``si`k.i``s``si`k.v``s``si`k.```s``si`k.```s``si`k.s``s``si`k.v``s``si`k.```s``si`k.```s``si`k.s``s``si`k.i``s``si`k.```s``si`k.k``s``si`k.```s``si`k.```s``si`k.s``s``si`k.i``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k..``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.k``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.i``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.s``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.s``s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.```s``si`k.```s``si`k.s``s``si`k.```s``si`k.k``s``si`k..``s``si`k.```s``si`k.vi"
)

(defn unl2sk [x] (map (fn [c] (if (= c \`) '_ (symbol (str c)))) x))
(defn sk2unl [x] (apply str (map (fn [s] (if (= s '_) "`" (str s))) x)))

(def sk-hello (unl2sk unl-hello))
(def sk-hello-n (unl2sk unl-hello-n))
(def sk-quine (unl2sk unl-quine))

(defn substo [e new a out]
  (conde
    [(nomo e) (== e a) (== new out)]
    [(nomo e) (!= e a) (== e out)]
    [(symbolo e) (== e out)]
    [(== e ()) (== e out)]
    [(fresh [h t ho to]
       (conso h t e)
       (conso ho to out)
       (substo h new a ho)
       (substo t new a to))]
    [(fresh [b bo]
       (nom/fresh [c]
         (== e (nom/tie c b))
         (== out (nom/tie c bo))
         (nom/hash c a)
         (nom/hash c new)
         (substo b new a bo)))]))

(defn doneo [in]
  (fresh [a d]
    (conso a d in)
    (!= a '.)
    (!= a '_)))

(defn evalo [in1 in2 out1 out2]
  (fresh [a d]
    (conso a d in1)
    (conde
      [(== a '.)
        (fresh [c r]
          (conso c r d)
          (symbolo c)
          (evalo (lcons `(~'. ~c) r) in2 out1 out2))]
      [(== a '_)
        (fresh [f rf arg rarg fun fbody sarg outf outf2 outarg rest]
          (evalo d (lcons f rf) out1 outf)
          (nom/fresh [x y z]
            (conde
              [(fresh [c] (== f `(~'. ~c)) (symbolo c) (== fun (nom/tie x `(~x))) (conso c outf2 outf))]
              [(== f 'i) (== fun (nom/tie x `(~x))) (== outf2 outf)]
              [(== f 'k) (== fun (nom/tie x `(~(nom/tie y `(~x))))) (== outf2 outf)]
              [(== f 's) (== fun (nom/tie x `(~(nom/tie y `(~(nom/tie z `(~'_ ~'_ ~x ~z ~'_ ~y ~z))))))) (== outf2 outf)]
              [(== f (nom/tie x fbody)) (== fun f) (== outf2 outf)])
            (== fun (nom/tie x fbody))
            (nom/hash x arg)
            (substo fbody arg x sarg))
          (evalo rf (lcons arg rarg) outf2 outarg)
          (appendo sarg rarg rest)
          (evalo rest in2 outarg out2))]
      [(doneo in1) (== in1 in2) (== out1 out2)])))
