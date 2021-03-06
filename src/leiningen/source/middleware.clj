(ns leiningen.source.middleware
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.nrepl.transport :as transport]
            [leiningen.source.analyzer :as analyzer]
            [leiningen.source.storage-provider :refer :all]
            [leiningen.source.storage-provider.textblockstore :refer [->FileStorage]])
  (:use [clojure.tools.nrepl.middleware :only [set-descriptor!]]
        [clojure.tools.nrepl.misc :only [response-for]])
  (:import clojure.tools.nrepl.transport.Transport))


(defn print-dup-str [x]
  (binding [*print-dup* true]
    (pr-str x)))

(defn print-with [out x]
  (binding [*out* out]
    (print x)
    (flush)))

;;
;; Utilities from Ritz
;;

(defmulti transform-value "Transform a value for output" type)

(defmethod transform-value :default [v] v)

(defmethod transform-value java.lang.Boolean [v]
  (if v "true" "false"))

(defmethod transform-value java.util.regex.Pattern [v]
  (str v))

(defmethod transform-value clojure.lang.PersistentVector [v]
  (list* (map transform-value v)))

(defmethod transform-value clojure.lang.LazySeq [v]
  (list* (map transform-value v)))

(defmethod transform-value clojure.lang.PersistentHashMap [m]
  (list* (mapcat #(vector (name (key %)) (transform-value (val %))) m)))

(defmethod transform-value clojure.lang.PersistentArrayMap [m]
  (list* (mapcat #(vector (name (key %)) (transform-value (val %))) m)))

(defn args-for-map
  "Return a value list based on a map. The keys are converted to strings."
  [m]
  (transform-value m))

(defn read-when
  "Read from the string passed if it is not nil"
  [s]
  (when s (read-string s)))



;;
;; Middleware and Handler
;;

(def backend (atom nil))
(def version (atom nil))

(defn read-handler [{:keys [sym transport] :as msg}]
  (let [results (->> (query @backend (str sym))
                     (map #(print-dup-str (:clj/form %))))]
    (transport/send
     transport (response-for msg
                             :value
                             (transform-value results)))
    (transport/send transport (response-for msg :status :done))))

(defn write-handler [{:keys [query transport] :as msg}]
  (let [results (->> (str query)
                     analyzer/analyze
                     (store @backend)
                     (map #(%))
                     dorun)]
    (transport/send
     transport (response-for msg
                             :value
                             (transform-value true)))
    (transport/send transport (response-for msg :status :done))))

(defn version-handler [{:keys [transport] :as msg}]
  (transport/send transport
                  (response-for msg
                                :value
                                (transform-value (str "\"" @version "\""))))
  (transport/send transport (response-for msg :status :done)))


(defn wrap-source [handler]
  (fn [{:keys [op] :as msg}]
    (case op
      "leiningen.source/read" (read-handler msg)
      "leiningen.source/write" (write-handler msg)
      "leiningen.source/version" (version-handler msg)
      (handler msg))))

(set-descriptor!
 #'wrap-source
 {:handles
  {"leiningen.source/read"
   {:doc "Return a list of forms matching the specified namespace or symbol."
    :requires {"sym" "The namespace qualified symbol to lookup"}
    :returns {"status" "done"}}}
  "leiningen.source/write"
  {:doc "Return a list of forms matching the specified namespace or symbol."
   :requires {"query" "The namespace qualified symbol to lookup"}
   :returns {"status" "done"}}
  "leiningen.source/version"
  {:doc "Returns a string of the current lein-source version number."
   :returns {"status" "done"}}})
