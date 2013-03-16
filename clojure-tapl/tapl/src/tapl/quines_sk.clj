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

(declare dsubsto)

(defn substo [e new a out1 out2]
  (conde
    [(nomo e) (== e a) (conso new out2 out1)]
    [(nomo e) (!= e a) (conso e out2 out1)]
    [(symbolo e) (conso e out2 out1)]
    [(== e ()) (conso e out2 out1)]
    [(fresh [h t oi]
       (conso h t e)
       (substo h new a out1 oi)
       (dsubsto t new a oi out2))]
    [(fresh [b bo]
       (nom/fresh [c]
         (== e (nom/tie c b))
         (conso (nom/tie c bo) out2 out1)
         (nom/hash c a)
         (nom/hash c new)
         (dsubsto b new a bo ())))]))

(defn dsubsto [e new a out1 out2]
  (conde
    [(== e ()) (== out2 out1)]
    [(fresh [h t oi]
       (conso h t e)
       (substo h new a out1 oi)
       (dsubsto t new a oi out2))]))

(defn doneo [in]
  (fresh [a d]
    (conso a d in)
    (!= a '_)))

(defn combo [h t l]
  (fresh [a d]
    (conso a d l)
    (conde
      [(== a '.)
       (fresh [a2]
         (conso a2 t d)
         (== `(~'. ~a2) h))]
      [(!= a '.) (== a h) (== d t)])))

(defn evalo [in lef1 out1 out2]
  (fresh [a d]
    (conso a d in)
    (conde
      [(== a '_)
        (fresh [f rf cf arg sarg rarg carg fun fbody outf1 outf2 outfo1 outfo2 outarg1 outarg2 outrest]
          (== out1 outf1)
          (== outf2 outarg1)
          (== outarg2 outfo1)
          (== outfo2 outrest)
          (evalo d cf outf1 outf2)
          (combo f rf cf)
          (nom/fresh [x y z]
            (conde
              [(fresh [c] (== f `(~'. ~c)) (symbolo c) (== fun (nom/tie x `(~x))) (conso c outfo2 outfo1))]
              [(== f 'i) (== fun (nom/tie x `(~x))) (== outfo2 outfo1)]
              [(== f 'k) (== fun (nom/tie x `(~(nom/tie y `(~x))))) (== outfo2 outfo1)]
              [(== f 's) (== fun (nom/tie x `(~(nom/tie y `(~(nom/tie z `(~'_ ~'_ ~x ~z ~'_ ~y ~z))))))) (== outfo2 outfo1)]
              [(== f 'v) (== fun (nom/tie x `(~'v))) (== outfo2 outfo1)]
              [(== f (nom/tie x fbody)) (== fun f) (== outfo2 outfo1)])
            (== fun (nom/tie x fbody))
            (nom/hash x arg)
            (evalo rf carg outarg1 outarg2)
            (combo arg rarg carg)
            (substo fbody arg x sarg rarg))
          (evalo sarg lef1 outrest out2))]
      [(doneo in) (== in lef1) (== out1 out2)])))
