(ns 
  ^{:doc "dynalint namespace containing alternate definitions for
Clojure 1.6.0 functions."}
  dynalint.clj160
  (:require [dynalint.checkers :as c]))


;; These functions are nearly identical to the versions in Clojure
;; 1.6.0 source code.  They were copied from there, and then edited
;; only to add invocations of argument-checking macros in namespace
;; dynalint.checkers, all with names of the form
;; dynalint.checkers/check-*


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def
 ^{:arglists '([x seq])
    :doc "Returns a new seq where x is the first element and seq is
    the rest."
   :added "1.0"
   :static true}

 cons (fn* ^:static cons [x seq]
         (c/check-cons x seq)
         (. clojure.lang.RT (cons x seq))))

(def
 ^{:arglists '([coll])
   :doc "Returns the first item in the collection. Calls seq on its
    argument. If coll is nil, returns nil."
   :added "1.0"
   :static true}
 first (fn ^:static first [coll]
         (c/check-first coll)
         (. clojure.lang.RT (first coll))))

(def
 ^{:arglists '([coll])
   :tag clojure.lang.ISeq
   :doc "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil."
   :added "1.0"
   :static true}  
 next (fn ^:static next [x]
        (c/check-next x)
        (. clojure.lang.RT (next x))))

(def
 ^{:arglists '([coll])
   :tag clojure.lang.ISeq
   :doc "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
   :added "1.0"
   :static true}  
 rest (fn ^:static rest [x]
        (c/check-rest x)
        (. clojure.lang.RT (more x))))

(def
 ^{:arglists '([coll x] [coll x & xs])
   :doc "conj[oin]. Returns a new collection with the xs
    'added'. (conj nil item) returns (item).  The 'addition' may
    happen at different 'places' depending on the concrete type."
   :added "1.0"
   :static true}
 conj (fn ^:static conj 
        ([coll x]
         (c/check-conj coll x)
         (. clojure.lang.RT (conj coll x)))
        ([coll x & xs]
         (c/check-conj coll x xs)
         (if xs
           (recur (conj coll x) (first xs) (next xs))
           (conj coll x)))))

(def
 ^{:arglists '(^clojure.lang.ISeq [coll])
   :doc "Returns a seq on the collection. If the collection is
    empty, returns nil.  (seq nil) returns nil. seq also works on
    Strings, native Java arrays (of reference types) and any objects
    that implement Iterable."
   :tag clojure.lang.ISeq
   :added "1.0"
   :static true}
 seq (fn ^:static seq ^clojure.lang.ISeq [coll]
       (c/check-seq coll)
       (. clojure.lang.RT (seq coll))))

(def
 ^{:arglists '([^clojure.lang.IObj obj m])
   :doc "Returns an object of the same type and value as obj, with
    map m as its metadata."
   :added "1.0"
   :static true}
 with-meta (fn ^:static with-meta [^clojure.lang.IObj x m]
             (c/check-with-meta x m)
             (. x (withMeta m))))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([] {})
  ([& keyvals]
   (c/check-hash-map keyvals)
   (. clojure.lang.PersistentHashMap (create keyvals))))

(defn sorted-map
  "keyval => key val
  Returns a new sorted map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([& keyvals]
   (c/check-sorted-map keyvals)
   (clojure.lang.PersistentTreeMap/create keyvals)))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  {:added "1.0"
   :static true}
  ([map]
   (c/check-dissoc map)
   map)
  ([map key]
   (c/check-dissoc map key)
   (. clojure.lang.RT (dissoc map key)))
  ([map key & ks]
   (c/check-dissoc map key ks)
   (let [ret (dissoc map key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  {:added "1.0"
   :static true}
  ([set]
   (c/check-disj set)
   set)
  ([^clojure.lang.IPersistentSet set key]
   (c/check-disj set key)
   (when set
     (. set (disjoin key))))
  ([set key & ks]
   (c/check-disj set key ks)
   (when set
     (let [ret (disj set key)]
       (if ks
         (recur ret (first ks) (next ks))
         ret)))))

(defn keys
  "Returns a sequence of the map's keys, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map]
  (c/check-keys map)
  (. clojure.lang.RT (keys map)))

(defn vals
  "Returns a sequence of the map's values, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map]
  (c/check-vals map)
  (. clojure.lang.RT (vals map)))

(defn key
  "Returns the key of the map entry."
  {:added "1.0"
   :static true}
  [^java.util.Map$Entry e]
    (c/check-key e)
    (. e (getKey)))

(defn val
  "Returns the value in the map entry."
  {:added "1.0"
   :static true}
  [^java.util.Map$Entry e]
    (c/check-val e)
    (. e (getValue)))

(defn rseq
  "Returns, in constant time, a seq of the items in rev (which
  can be a vector or sorted-map), in reverse order. If rev is empty returns nil"
  {:added "1.0"
   :static true}
  [^clojure.lang.Reversible rev]
    (c/check-rseq rev)
    (. rev (rseq)))

(defn name
  "Returns the name String of a string, symbol or keyword."
  {:tag String
   :added "1.0"
   :static true}
  [x]
  (c/check-name x)
  (if (string? x) x (. ^clojure.lang.Named x (getName))))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  {:tag String
   :added "1.0"
   :static true}
  [^clojure.lang.Named x]
    (c/check-namespace x)
    (. x (getNamespace)))

(defn remove-all-methods
  "Removes all of the methods of multimethod."
  {:added "1.2"
   :static true} 
 [^clojure.lang.MultiFn multifn]
 (c/check-remove-all-methods multifn)
 (.reset multifn))

(defn remove-method
  "Removes the method of multimethod associated with dispatch-value."
  {:added "1.0"
   :static true}
 [^clojure.lang.MultiFn multifn dispatch-val]
 (c/check-remove-method multifn dispatch-val)
 (. multifn removeMethod dispatch-val))

(defn prefer-method
  "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y 
   when there is a conflict"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn dispatch-val-x dispatch-val-y]
  (c/check-prefer-method multifn dispatch-val-x dispatch-val-y)
  (. multifn preferMethod dispatch-val-x dispatch-val-y))

(defn methods
  "Given a multimethod, returns a map of dispatch values -> dispatch fns"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn]
  (c/check-methods multifn)
  (.getMethodTable multifn))

(defn get-method
  "Given a multimethod and a dispatch value, returns the dispatch fn
  that would apply to that value, or nil if none apply and no default"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn dispatch-val]
  (c/check-get-method multifn dispatch-val)
  (.getMethod multifn dispatch-val))

(defn prefers
  "Given a multimethod, returns a map of preferred value -> set of other values"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn]
  (c/check-prefers multifn)
  (.getPreferTable multifn))

(defn find-var
  "Returns the global var named by the namespace-qualified symbol, or
  nil if no var with that name."
  {:added "1.0"
   :static true}
  [sym]
  (c/check-find-var sym)
  (. clojure.lang.Var (find sym)))

;; No arg checking for deref-future right now, but it is private in
;; clojure.core namespace, and I would prefer not to go through an
;; extra Var indirection to call it from modified deref below (TBD: Is
;; it an extra Var indirection to use #'clojure.core/deref-future in
;; deref below?)

(defn ^:private deref-future
  ([^java.util.concurrent.Future fut]
     (.get fut))
  ([^java.util.concurrent.Future fut timeout-ms timeout-val]
     (try (.get fut timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
          (catch java.util.concurrent.TimeoutException e
            timeout-val))))

(defn deref
  "Also reader macro: @ref/@agent/@var/@atom/@delay/@future/@promise. Within a transaction,
  returns the in-transaction-value of ref, else returns the
  most-recently-committed value of ref. When applied to a var, agent
  or atom, returns its current state. When applied to a delay, forces
  it if not already forced. When applied to a future, will block if
  computation not complete. When applied to a promise, will block
  until a value is delivered.  The variant taking a timeout can be
  used for blocking references (futures and promises), and will return
  timeout-val if the timeout (in milliseconds) is reached before a
  value is available. See also - realized?."
  {:added "1.0"
   :static true}
  ([ref]
     (c/check-deref ref)
     (if (instance? clojure.lang.IDeref ref)
       (.deref ^clojure.lang.IDeref ref)
       (deref-future ref)))
  ([ref timeout-ms timeout-val]
     (c/check-deref ref timeout-ms timeout-val)
     (if (instance? clojure.lang.IBlockingDeref ref)
       (.deref ^clojure.lang.IBlockingDeref ref timeout-ms timeout-val)
       (deref-future ref timeout-ms timeout-val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.set functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No arg checking for bubble-max-key.  See deref-future above for
;; reason.

(defn- bubble-max-key [k coll]
  "Move a maximal element of coll according to fn k (which returns a number) 
   to the front of coll."
  (let [max (apply max-key k coll)]
    (cons max (remove #(identical? max %) coll))))

(defn union
  "Return a set that is the union of the input sets"
  {:added "1.0"}
  ([] #{})
  ([s1]
     (c/check-union s1)
     s1)
  ([s1 s2]
     (c/check-union s1 s2)
     (if (< (count s1) (count s2))
       (reduce conj s2 s1)
       (reduce conj s1 s2)))
  ([s1 s2 & sets]
     (c/check-union s1 s2 sets)
     (let [bubbled-sets (bubble-max-key count (conj sets s2 s1))]
       (reduce into (first bubbled-sets) (rest bubbled-sets)))))
