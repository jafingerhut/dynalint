(ns 
  ^{:doc "dynalint namespace containing argument-checking macros for
specific functions in Clojure."}
  dynalint.checkers
  (:require [dynalint.utils :as util
             :refer [error-if error-if-not warn-if warn-if-not
                     warn short-ds seq-succeeds?]]))


(defmacro check-keys [map]
  `(do
     (error-if-not (seq-succeeds? ~map)
                   "First argument to clojure.core/keys must be seqable: "
                   (short-ds ~map))
     (when-not ((some-fn nil? map?) ~map)
       (warn "Calling clojure.core/keys with non-map: "
             (short-ds ~map)))))
