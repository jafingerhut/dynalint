(ns
  ^{:doc "dynalint namespace containing argument-checking macros for
specific functions in Clojure."}
  dynalint.checkers
  (:require [dynalint.utils :as util
             :refer [error error-if error-if-not
                     warn warn-if warn-if-not
                     short-ds seq-succeeds?]]))


;; Dynalint 0.1.3 only did check-nargs for the following functions,
;; where Clojure 1.5.1 and 1.6.0 already have what I consider to be a
;; reasonably clear error message if you call it with the wrong number
;; of arguments.

;; clojure.core/meta
;; clojure.core/seq?
;; clojure.core/map?
;; clojure.core/char?
;; clojure.core/string?
;; clojure.core/vector?
;; clojure.core/nil?

(defmacro check-keys [map]
  `(do
     (error-if-not (seq-succeeds? ~map)
       "First argument to clojure.core/keys must be seqable: "
       (short-ds ~map))
     (when-not ((some-fn nil? map?) ~map)
       (warn "Calling clojure.core/keys with non-map: "
             (short-ds ~map)))))

(defmacro check-vals [map]
  `(do
     (error-if-not (seq-succeeds? ~map)
       "First argument to clojure.core/vals must be seqable: "
       (short-ds ~map))
     (when-not ((some-fn nil? map?) ~map)
       (warn "Calling clojure.core/vals with non-map: "
             (short-ds ~map)))))

(defmacro check-key [e]
  `(do
     (error-if-not (instance? java.util.Map$Entry ~e)
       "First argument to clojure.core/key must be a map entry: "
       (short-ds ~e))))

(defmacro check-val [e]
  `(do
     (error-if-not (instance? java.util.Map$Entry ~e)
       "First argument to clojure.core/val must be a map entry: "
       (short-ds ~e))))

(defmacro check-rseq [rev]
  `(do
     (error-if-not (reversible? ~rev)
       "First argument to clojure.core/rseq must be reversible: "
       (short-ds ~rev))))

(defmacro check-name [x]
  `(do
     (error-if-not ((some-fn #(instance? clojure.lang.Named %) string?) ~x)
       "First argument to clojure.core/name must be string or named: "
       (short-ds ~x))))

(defmacro check-namespace [x]
  `(do
     (error-if-not ((some-fn #(instance? clojure.lang.Named %)) ~x)
       "First argument to clojure.core/namespace must be named: "
       (short-ds ~x))))

(defmacro check-remove-all-methods [multifn]
  `(do
     (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) ~multifn)
       "First argument to clojure.core/remove-all-methods must be a multimethod: "
       (short-ds ~multifn))))

(defmacro check-remove-method [multifn dispatch-val]
  `(do
     (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) ~multifn)
       "First argument to clojure.core/remove-method must be a multimethod: "
       (short-ds ~multifn))))

(defmacro check-prefer-method [multifn dispatch-val-x dispatch-val-y]
  `(do
     (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) ~multifn)
       "First argument to clojure.core/prefer-method must be a multimethod: "
       (short-ds ~multifn))))

(defmacro check-methods [multifn]
  `(do
     (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) ~multifn)
       "First argument to clojure.core/methods must be a multimethod: "
       (short-ds ~multifn))))

(defmacro check-get-method [multifn dispatch-val]
  `(do
     (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) ~multifn)
       "First argument to clojure.core/get-method must be a multimethod: "
       (short-ds ~multifn))))

(defmacro check-prefers [multifn]
  `(do
     (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) ~multifn)
       "First argument to clojure.core/prefers must be a multimethod: "
       (short-ds ~multifn))))

(defmacro check-find-var [sym]
  `(do
     (error-if-not (symbol? ~sym)
       "First argument to clojure.core/find-var must be a symbol: "
       (short-ds ~sym))
     (error-if-not (namespace ~sym)
       "First argument to clojure.core/find-var must be namespace qualified: "
       (short-ds ~sym))
     (error-if-not (find-ns (symbol (namespace ~sym)))
       "First argument to clojure.core/find-var must have a namespace that exists"
       " (no such namespace " (namespace ~sym) "): "
       (short-ds ~sym))))

;; TBD: Figure out how check-kw-params works, and how I want to handle
;; it here.
#_(defmacro check-agent [state options]
  `(do
     ;; TBD: Is this check-nargs more strict than the original function?
     ;;[& [i & opts :as all]]
     ;;(check-nargs #(< 0 %) this-var all)
     (check-kw-params
      this-var
      opts
      {:meta (check-iref-meta-kw this-var)
       :validator (check-iref-validator-kw this-var)
       :error-handler
       (fn [m present? kmap]
         (when present?
           (cond
             ;; false does not throw exception
             (false? m)
             (warn ":error-handler keyword argument to clojure.core/agent should be an ifn or nil: "
                   (short-ds m))
             (not ((some-fn nil? ifn?) m))
             (error ":error-handler keyword argument to clojure.core/agent must be ifn or nil: "
                    (short-ds m))))
         m)
       :error-mode
       (fn [m present? kmap]
         (when present?
           (cond
             ;; false and nil do not throw exceptions
             (or (false? m)
                 (nil? m)
                 (and (keyword? m)
                      (not (#{:continue :fail} m))))
             (warn ":error-mode keyword argument to clojure.core/agent should be keyword :continue or :fail: "
                   (short-ds m))
             (not (keyword? m))
             (error ":error-mode keyword argument to clojure.core/agent must be keyword :continue or :fail: "
                    (short-ds m))))
         m)
       })))

#_(defmacro check-ref [x]
  `(do
     ;;[& [i & opts :as all]]
     ;;(check-nargs #(< 0 %) this-var all)
     (check-kw-params
      this-var
      opts
      {:meta (check-iref-meta-kw this-var)
       :validator (check-iref-validator-kw this-var)
       :error-handler
       (fn [m present? kmap]
         (when present?
           (cond
             ;; false does not throw exception
             (false? m)
             (warn ":error-handler keyword argument to clojure.core/agent should be an ifn or nil: "
                   (short-ds m))
             (not ((some-fn nil? ifn?) m))
             (error ":error-handler keyword argument to clojure.core/agent must be ifn or nil: "
                    (short-ds m))))
         m)
       :error-mode
       (fn [m present? kmap]
         (when present?
           (cond
             ;; false and nil do not throw exceptions
             (or (false? m)
                 (nil? m)
                 (and (keyword? m)
                      (not (#{:continue :fail} m))))
             (warn ":error-mode keyword argument to clojure.core/agent should be keyword :continue or :fail: "
                   (short-ds m))
             (not (keyword? m))
             (error ":error-mode keyword argument to clojure.core/agent must be keyword :continue or :fail: "
                    (short-ds m))))
         m)
       })))

   ;; #'clojure.core/set-agent-send-executor!
   ;;  (fn clojure.core_SLASH_set-agent-send-executor!
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [exs :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (instance? java.util.concurrent.ExecutorService exs)
   ;;        "First argument to clojure.core/set-agent-send-off-executor! must be an executor service: "
   ;;        (short-ds exs))
   ;;      (apply original all)))
   ;; #'clojure.core/set-agent-send-off-executor!
   ;;  (fn clojure.core_SLASH_set-agent-send-off-executor!
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [exs :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (instance? java.util.concurrent.ExecutorService exs)
   ;;        "First argument to clojure.core/set-agent-send-off-executor! must be an executor service: "
   ;;        (short-ds exs))
   ;;      (apply original all)))

(defmacro check-dissoc
  ([m]
   `(do
      ;; The real dissoc just returns for 1 argument.
      (when (or (not (map? ~m))
                ;; if nil is passed, dissoc never throws
                (nil? ~m))
        (warn "clojure.core/dissoc first argument should be a map: "
              (short-ds ~m)))))
  ([m key & ks]
   `(do
      ;; Give a better error for more than 1 argument.  dissoc will
      ;; always fail if given anything other than a map or nil.
      (error-if-not (or (map? ~m) (nil? ~m))
        "clojure.core/dissoc first argument must be a map: "
        (short-ds ~m)))))

   ;; #'clojure.core/update-in
   ;;  (fn clojure.core_SLASH_update-in
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [m ks f & args :as all]]
   ;;      (check-nargs #(<= 3 %) this-var all)
   ;;      (when-not (coll? ks)
   ;;        (warn "clojure.core/update-in key path does not look like a collection: "
   ;;              (short-ds ks)))
   ;;      (when (coll? ks)
   ;;        (when-not (seq ks)
   ;;          (warn "clojure.core/update-in key path should be non-empty: "
   ;;                (short-ds ks))))
   ;;      (apply original all)))
   ;; #'clojure.core/assoc-in
   ;;  (fn clojure.core_SLASH_assoc-in
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [m ks v :as all]]
   ;;      (check-nargs #{3} this-var all)
   ;;      (warn-if (empty? ks)
   ;;        "clojure.core/assoc-in key path should be non-empty: "
   ;;        (short-ds ks))
   ;;      (apply original all)))
   ;; #'clojure.core/get-in
   ;;  (fn clojure.core_SLASH_get-in
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [m ks :as all]]
   ;;      (check-nargs #{2 3} this-var all)
   ;;      (error-if-not (seq-succeeds? ks)
   ;;        "Second argument to clojure.core/get-in must be seqable: "
   ;;        (short-ds ks))
   ;;      (when ((every-pred seq-succeeds? empty?)
   ;;             ks)
   ;;        (warn "clojure.core/get-in key path should be non-empty: "
   ;;              (short-ds ks)))
   ;;      (apply original all)))
   ;; #'clojure.core/select-keys
   ;;  (fn clojure.core_SLASH_select-keys
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [m keyseq :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (error-if-not (seq-succeeds? keyseq)
   ;;        "Second argument to clojure.core/select-keys must be seqable: "
   ;;        (short-ds keyseq))
   ;;      (when-not (instance? java.util.Map m)
   ;;        (if (or (nil? m) (empty? keyseq))
   ;;          (warn "clojure.core/select-keys first argument should be a map: "
   ;;                (short-ds m))
   ;;          (error "clojure.core/select-keys first argument must be a map: "
   ;;                 (short-ds m))))
   ;;      (apply original all)))
   ;; #'clojure.core/zipmap
   ;;  (fn clojure.core_SLASH_zipmap
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [ks vs :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (error-if-not (seq-succeeds? ks)
   ;;        "First argument to clojure.core/zipmap must be seqable: "
   ;;        (short-ds ks))
   ;;      (error-if-not (seq-succeeds? vs)
   ;;        "Second argument to clojure.core/zipmap must be seqable: "
   ;;        (short-ds vs))
   ;;      (when-not ((some-fn sequential? nil?) ks vs)
   ;;        (warn "clojure.core/zipmap arguments should be sequential or nil: "
   ;;              (short-ds ks) ", " (short-ds vs)))
   ;;      (apply original all)))
   ;; #'clojure.core/reverse
   ;;  (fn clojure.core_SLASH_reverse
   ;;    [original this-var]
   ;;    (fn wrapper[& [rev :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (seq-succeeds? rev)
   ;;        "First argument to clojure.core/reverse must be seqable: "
   ;;        (short-ds rev))
   ;;      (when (reversible? rev)
   ;;        (warn "clojure.core/reverse argument is reversible, consider clojure.core/rseq: "
   ;;              (short-ds rev)))
   ;;      (apply original all)))
   ;; #'clojure.core/unchecked-inc
   ;;   (fn clojure.core_SLASH_unchecked-inc
   ;;     [original this-var]
   ;;     (fn wrapper
   ;;       [& [x :as all]]
   ;;       (check-nargs #{1} this-var all)
   ;;       (let [res (apply original all)]
   ;;         (when-not (< x res)
   ;;           (warn "clojure.core/unchecked-inc overflow detected: "
   ;;                 (short-ds x) " (" (class x) ")" " -> " (short-ds res)
   ;;                 " (" (class x) ")"))
   ;;         res)))
   ;; #'clojure.core/unchecked-add
   ;;   (fn clojure.core_SLASH_unchecked-add
   ;;     [original this-var]
   ;;     (fn wrapper
   ;;       [& [x y :as all]]
   ;;       (check-nargs #{2} this-var all)
   ;;       ;FIXME should be inlined like unchecked-inc
   ;;       (let [res (cond
   ;;                   ; this case doesn't throw an exception, like the inline version.
   ;;                   ; This differs from the non-inline unchecked-add, which throws an
   ;;                   ; exception (it delegates to inc).
   ;;                   (and (instance? Long x)
   ;;                        (instance? Long y))
   ;;                     (let [^long x1 x
   ;;                           ^long y1 y]
   ;;                       ;(prn 'special-add)
   ;;                       (clojure.lang.Numbers/unchecked_add x y))
   ;;                   :else (clojure.lang.Numbers/unchecked_add x y))]
   ;;         (when-not (< (+ x y) res)
   ;;           (warn "clojure.core/unchecked-add overflow detected: "
   ;;                 (short-ds x) " (" (class x) ") + "
   ;;                 (short-ds y) " (" (class y) ") -> "
   ;;                 (short-ds res) " (" (class x) ")"))
   ;;         res)))

(defmacro check-union
  ([s1]
   `(do
      (when-not (set? ~s1)
        (warn "clojure.set/union should have set arguments: "
              (short-ds ~s1)))))
  ([s1 s2]
   `(do
      (when-not (set? ~s1)
        (warn "clojure.set/union should have set arguments: "
              (short-ds ~s1)))
      (when-not (set? ~s2)
        (warn "clojure.set/union should have set arguments: "
              (short-ds ~s2)))))
  ([s1 s2 sets]
   `(do
      (when-not (set? ~s1)
        (warn "clojure.set/union should have set arguments: "
              (short-ds ~s1)))
      (when-not (set? ~s2)
        (warn "clojure.set/union should have set arguments: "
              (short-ds ~s2)))
      (doseq [s# (remove set? ~sets)]
        (warn "clojure.set/union should have set arguments: "
              (short-ds s#))))))

   ;; #'clojure.set/intersection
   ;;  (fn clojure.set_SLASH_intersection
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [:as all]]
   ;;      (check-nargs #(<= 1 %) this-var all)
   ;;      (when-let [non-sets (seq (filter (complement set?) all))]
   ;;        (doseq [s non-sets]
   ;;          (warn "clojure.set/intersection should have set arguments: "
   ;;                (short-ds s))))
   ;;      (apply original all)))
   ;; #'clojure.set/difference
   ;;  (fn clojure.set_SLASH_difference
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [:as all]]
   ;;      (check-nargs #(<= 1 %) this-var all)
   ;;      (when-let [non-sets (seq (filter (complement set?) all))]
   ;;        (doseq [s non-sets]
   ;;          (warn "clojure.set/difference should have set arguments: "
   ;;                (short-ds s))))
   ;;      (apply original all)))
   ;; #'clojure.set/select
   ;;  (fn clojure.set_SLASH_select
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [pred xset :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (when-not (set? xset)
   ;;        (warn "clojure.set/select should have set for second argument: "
   ;;              (short-ds xset)))
   ;;      (apply original all)))
   ;; #'clojure.set/project
   ;;  (fn clojure.set_SLASH_project
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [xrel ks :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (when-not (map? xrel)
   ;;        (warn "clojure.set/project first argument should be map: "
   ;;              (short-ds xrel)))
   ;;      (apply original all)))
   ;; #'clojure.set/rename-keys
   ;;  (fn clojure.set_SLASH_rename-keys
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [m kmap :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (when-not (map? m)
   ;;        (warn "clojure.set/rename-keys first argument should be map: "
   ;;              (short-ds m)))
   ;;      (when-not (map? kmap)
   ;;        (warn "clojure.set/rename-keys second argument should be map: "
   ;;              (short-ds kmap)))
   ;;      (apply original all)))
   ;; #'clojure.set/rename
   ;;  (fn clojure.set_SLASH_rename
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [xrel kmap :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (when-not (rel? xrel)
   ;;        (warn "clojure.set/rename first argument should be a relation: "
   ;;              (short-ds xrel)))
   ;;      (when-not (map? kmap)
   ;;        (warn "clojure.set/rename second argument should be a map: "
   ;;              (short-ds kmap)))
   ;;      (apply original all)))
   ;; #'clojure.set/index
   ;;  (fn clojure.set_SLASH_index
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [xrel ks :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (when-not (rel? xrel)
   ;;        (warn "clojure.set/index first argument should be a relation: "
   ;;              (short-ds xrel)))
   ;;      (apply original all)))
   ;; #'clojure.set/map-invert
   ;;  (fn clojure.set_SLASH_map-invert
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [m :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (when-not (map? m)
   ;;        (warn "clojure.set/map-invert first argument should be map: "
   ;;              (short-ds m)))
   ;;      (apply original all)))
   ;; #'clojure.set/join
   ;;  (fn clojure.set_SLASH_join
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [xrel yrel km :as all]]
   ;;      (check-nargs #{2 3} this-var all)
   ;;      (when (#{2 3} (count all))
   ;;        (when-not (rel? xrel)
   ;;          (warn "clojure.set/join first argument should be a relation: "
   ;;                (short-ds xrel)))
   ;;        (when-not (rel? yrel)
   ;;          (warn "clojure.set/join second argument should be a relation: "
   ;;                (short-ds yrel))))
   ;;      (when (#{3} (count all))
   ;;        (when-not (map? km)
   ;;          (warn "clojure.set/join third argument should be a map: "
   ;;                (short-ds km))))
   ;;      (apply original all)))
   ;; #'clojure.set/subset?
   ;;  (fn clojure.core_SLASH_subset?
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [:as all]]
   ;;      (when-let [non-sets (seq (filter (complement set?) all))]
   ;;        (doseq [s non-sets]
   ;;          (warn "clojure.set/subset? should have set arguments: "
   ;;                (short-ds s))))
   ;;      (apply original all)))
   ;; #'clojure.set/superset?
   ;;  (fn clojure.set_SLASH_superset?
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [:as all]]
   ;;      (when-let [non-sets (seq (filter (complement set?) all))]
   ;;        (doseq [s non-sets]
   ;;          (warn "clojure.set/superset? should have set arguments: "
   ;;                (short-ds s))))
   ;;      (apply original all)))

(defmacro check-with-meta [x m]
  `(do
     (error-if-not (instance? clojure.lang.IMeta ~x)
       "First argument to clojure.core/with-meta must implement clojure.lang.IMeta: "
       (short-ds ~x))
     (error-if-not (or (map? ~m) (nil? ~m))
       "Second argument to clojure.core/with-meta must be a map or nil: "
       (short-ds ~m))))

   ;; #'clojure.core/last
   ;;  (fn clojure.core_SLASH_last
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [the-seq :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (seq-succeeds? the-seq)
   ;;        "First argument to clojure.core/last must be seqable: "
   ;;        (short-ds the-seq))
   ;;      (original the-seq)))
   ;; #'clojure.core/butlast
   ;;  (fn clojure.core_SLASH_butlast
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [the-seq :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (seq-succeeds? the-seq)
   ;;        "First argument to clojure.core/butlast must be seqable: "
   ;;        (short-ds the-seq))
   ;;      (original the-seq)))

(defmacro check-cons [x the-seq]
  `(do
     (error-if-not (seq-succeeds? ~the-seq)
       "Second argument to clojure.core/cons must be seqable: "
       (short-ds ~the-seq)
       " (instance of " (class ~the-seq) ")")))

(defmacro check-first [coll]
  `(do
     (error-if-not (seq-succeeds? ~coll)
       "First argument to clojure.core/first must be seqable: "
       (short-ds ~coll)
       " (instance of " (class ~coll) ")")))

   ;; #'clojure.core/second
   ;;  (fn clojure.core_SLASH_second
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [the-seq :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (seq-succeeds? the-seq)
   ;;        "First argument to clojure.core/second must be seqable: "
   ;;        (short-ds the-seq))
   ;;      (original the-seq)))
   ;; #'clojure.core/ffirst
   ;;  (fn clojure.core_SLASH_ffirst
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [the-seq :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (seq-succeeds? the-seq)
   ;;        "First argument to clojure.core/ffirst must be seqable: "
   ;;        (short-ds the-seq))
   ;;      (error-if-not (seq-succeeds? (first the-seq))
   ;;        "First argument to clojure.core/ffirst must have seqable first element: "
   ;;        (short-ds (first the-seq)))
   ;;      (original the-seq)))
   ;; #'clojure.core/nfirst
   ;;  (fn clojure.core_SLASH_nfirst
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [the-seq :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (seq-succeeds? the-seq)
   ;;        "First argument to clojure.core/nfirst must be seqable: "
   ;;        (short-ds the-seq))
   ;;      (error-if-not (seq-succeeds? (first the-seq))
   ;;        "First argument to clojure.core/nfirst must have seqable first element: "
   ;;        (short-ds (first the-seq)))
   ;;      (original the-seq)))
   ;; #'clojure.core/fnext
   ;;  (fn clojure.core_SLASH_fnext
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [the-seq :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (seq-succeeds? the-seq)
   ;;        "First argument to clojure.core/fnext must be seqable: "
   ;;        (short-ds the-seq))
   ;;      (original the-seq)))
   ;; #'clojure.core/nnext
   ;;  (fn clojure.core_SLASH_nnext
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [the-seq :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (seq-succeeds? the-seq)
   ;;        "First argument to clojure.core/nnext must be seqable: "
   ;;        (short-ds the-seq))
   ;;      (original the-seq)))


   ;; ; this is often a compile time check anyway
   ;; #'clojure.core/instance?
   ;;  (fn clojure.core_SLASH_instance?
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [cls x :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (error-if-not (class? cls)
   ;;        "First argument to clojure.core/instance? must be a Class: "
   ;;        (short-ds cls))
   ;;      (apply original all)))

(defmacro check-seq [coll]
  `(do
     ;; apply uses seq, results in infinite cycles
     (error-if-not (seq-succeeds? ~coll)
       "First argument to clojure.core/seq must be seqable: "
       (short-ds ~coll))))

   ;; #'clojure.core/symbol
   ;;  (fn clojure.core_SLASH_symbol
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [:as all]]
   ;;      (check-nargs #{1 2} this-var all)
   ;;      (case (count all)
   ;;        1
   ;;          (let [[s1] all]
   ;;            (when-not ((some-fn string? symbol?) s1)
   ;;              (error "First argument to clojure.core/symbol (with 1 argument) must be a string or symbol: "
   ;;                     (short-ds s1))))
   ;;        2
   ;;          (let [[s1 s2] all]
   ;;            (when-not (string? s1)
   ;;              (error "First argument to clojure.core/symbol (with 2 arguments) must be a string: "
   ;;                     (short-ds s1)))
   ;;            (when-not (string? s2)
   ;;              (error "Second argument to clojure.core/symbol (with 2 arguments) must be a string: "
   ;;                     (short-ds s2)))))
   ;;      (apply original all)))
   ;; #'clojure.core/cast
   ;;  (fn clojure.core_SLASH_cast
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [c i :as all]]
   ;;      (check-nargs #{2} this-var all)
   ;;      (error-if-not (class? c)
   ;;        "First argument to clojure.core/cast must be a class: "
   ;;        (short-ds c))
   ;;      (original c i)))
   ;; #'clojure.core/to-array
   ;;  (fn clojure.core_SLASH_to-array
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [coll :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (to-array-succeeds? coll)
   ;;        "First argument to clojure.core/to-array must be a collection: "
   ;;        (short-ds coll))
   ;;      (apply original all)))
   ;; #'clojure.core/vec
   ;;  (fn clojure.core_SLASH_vec
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [coll :as all]]
   ;;      (check-nargs #{1} this-var all)
   ;;      (error-if-not (or (instance? java.util.Collection coll) (to-array-succeeds? coll))
   ;;        "First argument to clojure.core/vec must be a collection or array: "
   ;;        (short-ds coll))
   ;;      (original coll)))

(defmacro check-hash-map [keyvals]
  `(do
     (error-if-not (even? (count ~keyvals))
       "Must pass even number of arguments to clojure.core/hash-map, actual: "
       (count ~keyvals))))

(defmacro check-sorted-map [keyvals]
  `(do
     (error-if-not (even? (count ~keyvals))
       "Must pass even number of arguments to clojure.core/sorted-map, actual: "
       (count ~keyvals))))

   ;; #'clojure.core/sorted-map-by
   ;;  (fn clojure.core_SLASH_sorted-map-by
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [c & args :as all]]
   ;;      (check-nargs #(<= 1 %) this-var all)
   ;;      (error-if-not (instance? java.util.Comparator c)
   ;;        "First argument to clojure.core/sorted-map-by must be a comparator, actual: "
   ;;        (count args))
   ;;      (error-if-not (even? (count args))
   ;;        "Must pass even number of variable arguments to clojure.core/sorted-map-by, actual: "
   ;;        (count args))
   ;;      (apply original all)))
   ;; #'clojure.core/sorted-set-by
   ;;  (fn clojure.core_SLASH_sorted-set-by
   ;;    [original this-var]
   ;;    (fn wrapper
   ;;      [& [c & args :as all]]
   ;;      (check-nargs #(<= 1 %) this-var all)
   ;;      (error-if-not (instance? java.util.Comparator c)
   ;;        "First argument to clojure.core/sorted-set-by must be a comparator, actual: "
   ;;        (count args))
   ;;      (apply original all)))
   ;; #'clojure.core/find
   ;;  (fn clojure.core_SLASH_find
   ;;    [original the-var]
   ;;    (fn wrapper
   ;;      [& [m k :as all]]
   ;;      (check-nargs #{2} the-var all)
   ;;      (error-if-not ((some-fn #(instance? java.util.Map %) nil?) m)
   ;;        "First argument to clojure.core/find must be a map or nil: " (short-ds m))
   ;;      (original m k)))

(defmacro check-disj
  ([set]
   `(do
      (when-not (or (set? ~set) (nil? ~set))
        (warn "First argument to clojure.core/disj should be a set or nil: "
              (short-ds ~set)))))
  ([set key & ks]
   `(do
      (when-not (or (set? ~set) (nil? ~set))
        (if (false? ~set)
          ;; false doesn't throw a runtime error
          (warn "First argument to clojure.core/disj should be a set or nil: "
                (short-ds ~set))
          (error "First argument to clojure.core/disj must be a set or nil: "
                 (short-ds ~set)))))))

;; TBD: Currently check-conj only checks args coll and x.  It
;; could/should check xs as well, similarly to x.
(defmacro check-conj [coll x & xs]
  `(do
     (cond
       (not (or (nil? ~coll) (coll? ~coll)))
       (error "First argument to clojure.core/conj must be a persistent collection or nil: "
              (short-ds ~coll))
       (and (map? ~coll)
            (not
             (or
              (instance? java.util.Map$Entry ~x)
              (and (vector? ~x) (== 2 (count ~x)))
              (and (seq-succeeds? ~x)
                   (every? #(instance? java.util.Map$Entry %) ~x))
              (nil? ~x))))
       (error "Can only conj nil, a map entry, a vector pair or a seqable of map entries onto a map: "
              (short-ds ~x)))))

(defmacro check-next [x]
  `(do
     (error-if-not (seq-succeeds? ~x)
       "First argument to clojure.core/next must be seqable: "
       (short-ds ~x))))

(defmacro check-rest [x]
  `(do
     (error-if-not (seq-succeeds? ~x)
       "First argument to clojure.core/rest must be seqable: "
       (short-ds ~x))))

  ; TODO some complicated invariants and error conditions with reduce
;   #'clojure.core/reduce
;    (fn clojure.core_SLASH_reduce
;      [original the-var]
;      (fn wrapper
;        [& [:as all]]
;        (check-nargs #{2 3} the-var all)
;        (apply original all)))

(defmacro check-deref
  ([the-ref]
   `(do
      (when-not (or (instance? clojure.lang.IDeref ~the-ref)
                    (instance? java.util.concurrent.Future ~the-ref))
        (error "First argument to clojure.core/deref must be IDeref or a future: "
               (short-ds ~the-ref)))))
  ([the-ref timeout-ms timeout-val]
   `(do
      (when-not (or (instance? clojure.lang.IBlockingDeref ~the-ref)
                    (instance? java.util.concurrent.Future ~the-ref))
        (error "First argument to clojure.core/deref must be IBlockingDeref or a future: "
               (short-ds ~the-ref))))))
