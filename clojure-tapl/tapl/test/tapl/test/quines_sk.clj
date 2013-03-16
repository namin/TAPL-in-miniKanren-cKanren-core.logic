(ns tapl.test.quines_sk
  (:use [tapl.quines_sk] :reload)
  (:use [tapl.utils])
  (:use [clojure.test])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic.protocols]
        [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(deftest test-conversion-sanity
  (is (= unl-quine (sk2unl (unl2sk unl-quine)))))

(deftest test-basics
  (is (= (run* [q] (fresh [lef1 out1] (evalo '(_ . h i) lef1 out1 ()) (== q [lef1 out1])))
        '([(i) (h)])))
  (is (= (run* [q] (fresh [lef1 out1] (evalo '(_ _ . h . e i) lef1 out1 ()) (== q [lef1 out1])))
        '([(i) (h e)]))))

(deftest test-hello
  (is (= (map #(apply str %)
           (run* [q] (evalo sk-hello '(i) q ())))
        '("Hello world"))))

(deftest test-sk
  (is (= (run* [q] (fresh [lef1 out1] (evalo '(_ _ _ s k s i) lef1 out1 ()) (== q [lef1 out1])))
        '([(i) ()])))
  (is (= (run* [q] (fresh [lef1 out1] (evalo '(_ _ k i s) lef1 out1 ()) (== q [lef1 out1])))
        '([(i) ()]))))

(deftest test-v
  (is (= (run* [q] (fresh [lef1 out1] (evalo '(_ _ _ s v i i) lef1 out1 ()) (== q [lef1 out1])))
        '([(v) ()]))))

(deftest test-iter
  (is (= (run* [q] (fresh [lef1 out1] (evalo (unl2sk "```si`k``s.*``sii``si``si``si``si``si``si``si``si`ki") lef1 out1 ()) (== q [lef1 out1])))
        '([(i) (* * * * * * * *)])))
  (is (= (run* [q] (fresh [lef1 out1] (evalo (unl2sk "```si`k``s.*``sii``si``si`ki") lef1 out1 ()) (== q [lef1 out1])))
        '([(i) (* *)]))))

(deftest test-hello-n
  (is (= (map #(apply str %)
           (run* [q] (fresh [i o1 o2] (evalo sk-hello-n '(i) q ()))))
        '("Hello world!Hello world!Hello world!Hello world!Hello world!Hello world!Hello world!Hello world!"))))

(defn flatto [v]
  (fixc v
    (fn [t _ _]
      (cond
        (sequential? t) (if (empty? t) succeed (composeg (symbolo (first t)) (flatto (rest t))))
        (lcons? t) (composeg (symbolo (lfirst t)) (flatto (lnext t)))
        :else fail))
    (fn [_ v _ r a]
      `(~'flatto ~(-reify a v r)))))

(deftest test-backwards-1
  (is (= (run 10 [q] (flatto q) (evalo q '(i) '(y o) ()))
          ;; all boring!
        '( (_ . y _ . o i)
           (_ i _ . y _ . o i)
           (_ . y _ i _ . o i)
           (_ . y _ . o _ i i)
           (_ i _ i _ . y _ . o i)
           (_ i _ . y _ i _ . o i)
           (_ . y _ i _ i _ . o i)
           (_ i _ . y _ . o _ i i)
           (_ . y _ i _ . o _ i i)
           (_ . y _ . o _ i _ i i)))))

(deftest test-backwards-2
  (is (= (run 3 [q] (fresh [i o end] (appendo '(_ _ _ s k) end q) (evalo q '(i) () ())))
        ;; it stalls without the k                   here
        '((_ _ _ s k i i) (_ _ _ s k k i) (_ _ _ s k v i)))))
