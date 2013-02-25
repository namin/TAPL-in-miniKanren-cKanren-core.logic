(ns tapl.utils
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.nominal :exclude [fresh hash] :as nom]))

(defn without-constraints [r]
  (if (= ':- (second r))
    (first r)
    r))

