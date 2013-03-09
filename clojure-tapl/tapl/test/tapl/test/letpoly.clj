(ns tapl.test.letpoly
  (:use [tapl.letpoly] :reload)
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom])
  (:use [clojure.test]))

(deftest test-stlc
  (is (= (run* [t]
           (nom/fresh [a]
             (typeofo env-empty ['lam (nom/tie a ['var a])] t)))
        '([TArrow _0 _0])))
  (is (= (run* [t]
            (nom/fresh [a]
              (typeofo env-empty ['app ['lam (nom/tie a ['var a])] ['lam (nom/tie a ['var a])]] t)))
        '([TArrow _0 _0])))
  (is (= (run* [t]
            (nom/fresh [a b]
              (typeofo env-empty ['lam (nom/tie a ['lam (nom/tie b ['app ['var a] ['var b]])])] t)))
        '([TArrow [TArrow _0 _1] [TArrow _0 _1]])))
  (is (= (run* [t]
          (nom/fresh [f a d]
            (typeofo env-empty ['lam (nom/tie f ['lam (nom/tie a ['app ['lam (nom/tie d ['var f])] ['app ['var f] ['var a]]])])] t)))
        '([TArrow [TArrow _0 _1] [TArrow _0 [TArrow _0 _1]]]))))

(deftest test-let
  (is (= (run* [t]
            (nom/fresh [a b]
              (typeofo env-empty ['let ['lam (nom/tie a ['var a])] (nom/tie a ['var a])] t)))
        '([TArrow _0 _0])))
  (is (= (run* [t]
            (nom/fresh [a b]
              (typeofo env-empty ['let ['lam (nom/tie a ['var a])] (nom/tie a ['app ['var a] ['var a]])] t)))
        '([TArrow _0 _0])))
  (is (= (run* [t]
            (nom/fresh [a b]
              (typeofo env-empty ['lam (nom/tie a ['let ['var a] (nom/tie b ['var b])])] t)))
        '([TArrow _0 _0])))
  (is (= (run* [t]
           (nom/fresh [f a d id x]
             (typeofo env-empty ['lam (nom/tie f ['lam (nom/tie a ['let ['lam (nom/tie x ['var x])] (nom/tie id ['app ['lam (nom/tie d ['app ['var id] ['var f]])] ['app ['app ['var id] ['var f]] ['app ['var id] ['var a]]]])])])] t)))
        '([TArrow [TArrow _0 _1] [TArrow _0 [TArrow _0 _1]]])))
  (is (= (run* [t]
           (nom/fresh [f a d id]
             (typeofo env-empty ['lam (nom/tie f ['lam (nom/tie a ['let ['lam (nom/tie a ['var a])] (nom/tie id ['app ['lam (nom/tie d ['app ['var id] ['var f]])] ['app ['app ['var id] ['var f]] ['app ['var id] ['var a]]]])])])] t)))
        '([TArrow [TArrow _0 _1] [TArrow _0 [TArrow _0 _1]]]))))
