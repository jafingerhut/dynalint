(ns dynalint.utils
  (:require [clojure.repl :as repl]
            [clojure.string :as str]))


(def error-history
  (atom (sorted-map)))

(def warning-history
  (atom (sorted-map)))

(defn add-error [id e]
  (swap! error-history assoc id e))

(defn add-warning [id e]
  (swap! warning-history assoc id e))

(def id (atom 0))

(defn inc-id []
  (swap! id inc))

(declare prettify-stack-trace drop-until-stack-entry)

(defn error [& args]
;;  (println "dbg: error 'dynalint.utils/error")
  (let [id (inc-id)
        msg (apply str "ERROR " (str "(Dynalint id " id "): ") args)
        e (->
            (try
              (throw
                (ex-info
                  msg
                  {:dynalint.lint/dynalint true
                   :dynalint.lint/error true
                   :dynalint.lint/id id}))
              (catch clojure.lang.ExceptionInfo e
                e))
            ; we want dynalint.utils/error at the top
            (prettify-stack-trace 
              :process-entries (drop-until-stack-entry 'dynalint.utils/error)))]
    (add-error id e)
    (throw e)))

(declare errors-disabled?)

(def ^:dynamic *disable-warnings-in* false)
(def ^:dynamic *disable-errors-in* false)

(defmacro with-disabled-linting 
  [& args]
  `(do
     (push-thread-bindings
       {#'*disable-warnings-in* true
        #'*disable-errors-in* true})
     (try
       (do ~@args)
       (finally
         (pop-thread-bindings)))))

(defmacro when-errors-enabled [& args]
  `(when-not (errors-disabled?)
     (with-disabled-linting
       ~@args)))

(defmacro when-warnings-enabled [& args]
  `(when-not (warnings-disabled?)
     (with-disabled-linting
       ~@args)))

(defmacro warn-if [p & args]
  `(when-warnings-enabled
     (when ~p
       (warn ~@args))))

(defmacro warn-if-not [p & args]
  `(when-warnings-enabled
     (when-not ~p
       (warn ~@args))))

(defmacro error-if [p & args]
  `(when-errors-enabled
     (when ~p
       (error ~@args))))

(defmacro error-if-not [p & args]
  `(when-errors-enabled
     (when-not ~p
       (error ~@args))))

;(t/ann ^:no-check relevant-stacktrace [-> Any])
(defn ^:private relevant-stacktrace [& {:keys [verbose]}]
  (try (throw (Exception. ""))
       (catch Exception e
         (let [epst (with-out-str
                      (binding [*err* *out*]
                        (repl/pst e 25)))]
           (or
             (when-not verbose
               (->>
                 epst
                 str/split-lines
                 (filter (fn [^String s]
                           (.contains s (str (ns-name *ns*) "/"))))
                 doall
                 seq))
             (str/split-lines
               epst))))))

;used in macros
;(t/ann ^:no-check print-relevant-stacktrace [& :optional {:verbose Any} -> Any])
(defn ^:skip-wiki print-relevant-stacktrace [& {:keys [verbose]}]
  (doseq [r (relevant-stacktrace :verbose verbose)]
    (println r))
  (flush))

(defn prettify-stack-trace [^Throwable e & {:keys [process-entries]}]
;;  (println "dbg: prettify-stack-trace start")
;;  (repl/pst e 200)
;;  (println "dbg: prettify-stack-trace end dbg")
  (letfn [(wrapper-marker? [^String s]
            {:pre [(or (nil? s) (string? s))]}
            ;(prn "wrapper-marker?" s)
            (when s
              (.startsWith s "wrapper")))
          (ste-for-var [^clojure.lang.Var v]
            {:pre [(var? v)]
             :post [(or (nil? %)
                        (instance? StackTraceElement %))]}
            (let [{:keys [ns name line file]} (meta v)]
              (let [file-name (when file 
                                (last (str/split file #"/")))]
                ;(prn "file-name" file-name)
                (when (and ns name line file-name)
                  (StackTraceElement.
                    (str (namespace-munge ns) "$" (.sym v))
                    (str 'invoke)
                    file-name
                    line)))))
          (process-ste [^StackTraceElement ste]
            {:pre [(instance? StackTraceElement ste)]
             :post [(or (nil? %)
                        (instance? StackTraceElement %))]}
            (let [clstr (.getClassName ste)
                  [nstr wrapper-of innermarker right-more :as allcls]
                  (->> (partition-by #{\$} clstr)
                       (map (partial apply str))
                       (remove #{"$"}))
                 ; _ (prn "in dynalint marker" allcls)
                 ; _ (prn "original clstr" clstr)
                 ; _ (prn "nstr" nstr)
                 ; _ (prn "wrapper-of" wrapper-of)
                 ; _ (prn "innermarker" innermarker)
                  ]
              (if-not (and (= "dynalint.lint" nstr)
                           (wrapper-marker? innermarker))
                ste
                (let [;_ (prn "is a wrapper..")
                      [munged-ns munged-name] (str/split wrapper-of #"_SLASH_")
                      demunged-ns (str (str/replace munged-ns #"_DOT_" "."))
                      demunged-name (str (repl/demunge munged-name))
                      wrapping-varsym (symbol demunged-ns demunged-name)
                      ;_ (prn "demunged varsym" wrapping-varsym)
                      v (when (find-ns (symbol demunged-ns))
                          (find-var wrapping-varsym))
                      ;_ (prn ".. of var" v)
                      ]
                  (if-not (var? v)
                    ste 
                    (cond
                      ; drop any inner calls inside a dynalint wrapper
                      (seq right-more) nil
                      :else (or (ste-for-var v)
                                ste)))))))]
    (doto e
      (.setStackTrace
        (into-array 
          StackTraceElement
          (let [original-st (.getStackTrace e)
                ;_ (prn "original-st" (map (comp :className bean) original-st))
                es (->> original-st
                     (map process-ste)
                     (remove nil?))]
            ((or process-entries identity) es)))))))

(defn drop-until-stack-entry [clsym]
  #(drop-while 
     (fn [^StackTraceElement el]
       (not= (repl/demunge (.getClassName el))
             (str clsym)))
     %))

(def ^clojure.lang.Atom disable-warnings 
  "If set to true, skip all warning checks installed by Dynalint.
  Defaults to false"
  (atom false))
(def ^clojure.lang.Atom disable-errors 
  "If set to true, skip all error checks installed by Dynalint.
  Defaults to false"
  (atom false))

(defn reset-globals! [opts val]
  (doseq [o opts]
    (case o
      :warn (reset! disable-warnings val)
      :error (reset! disable-errors val)
      :all (do (reset! disable-warnings val)
               (reset! disable-errors val))
      nil)))

(defn errors-disabled? []
  (or 
    ; must be very careful calling c.c/deref here
    (.deref disable-errors)
    *disable-errors-in*))

(defn warnings-disabled? []
  (or 
    ; must be very careful calling c.c/deref here
    (.deref disable-warnings)
    *disable-warnings-in*))

(def last-warning-nano-time (atom Long/MIN_VALUE))

(def warning-interval
  "Miniumum number of seconds between warnings."
  (atom 1))

(defn should-throw-warning? []
  (let [curr (System/nanoTime)
        next-warning-time (+' (*' 1e9 @warning-interval) @last-warning-nano-time)]
    (if (< next-warning-time curr)
      (do (reset! last-warning-nano-time curr)
          true)
      false)))

;used in macros
;(t/ann warn [Any * -> Any])
(defn ^:skip-wiki warn [& args]
  (when (should-throw-warning?)
;;    (println "dbg: warn 'dynalint.utils/warn")
    (let [id (inc-id)
          msg (print-str "WARNING" (str "(Dynalint id " id "): ") 
                         (apply str args))
          e (->
              (try (throw (ex-info
                            msg
                            {:dynalint.lint/dynalint true
                             :dynalint.lint/warning true
                             :dynalint.lint/id id}))
                   (catch clojure.lang.ExceptionInfo e
                     e))
              ; we want dynalint.utils/warn at the top
              (prettify-stack-trace 
                :process-entries (drop-until-stack-entry 'dynalint.utils/warn)))]
      (add-warning id e)
      (println msg)
      (flush))))

;used in macros
;(t/ann ^:no-check short-ds [Any -> Any])
(defn ^:skip-wiki short-ds [ds]
  (with-out-str
    (binding [*print-length* 3
              *print-level*  3]
      (pr ds)
      (flush))))

;(t/ann rel? [Any -> Any])
(defn rel? [a]
  (and (set? a)
       (every? map? a)))

(defmacro throws-exception? [& body]
  `(try (do ~@body false)
        (catch Throwable _#
          true)))

;(t/ann seq-succeeds? [Any -> Any])
(defn seq-succeeds? [s]
  (or (instance? clojure.lang.Seqable s)
      (nil? s)
      (instance? Iterable s)
      (let [^Class c (class s)]
        (.isArray c))
      (instance? CharSequence s)
      (instance? java.util.Map s)))

(defn to-array-succeeds? [s]
  (or (nil? s)
      (instance? (Class/forName "[Ljava.lang.Object;")
                 s)
      (instance? java.util.Collection s)
      (instance? java.util.Map s)
      (string? s)
      (let [^Class c (class s)]
        (.isArray c))))
