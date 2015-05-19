(ns 
  ^{:doc "dynalint namespace containing alternate definitions for
Clojure 1.6.0 functions."}
  dynalint.clj160
  (:require [dynalint.checkers :as c]))


;; These functions are nearly identical to the versions in Clojure
;; 1.6.0 source code.


(defn keys
  "Returns a sequence of the map's keys, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map]
  (c/check-keys map)
  (. clojure.lang.RT (keys map)))
