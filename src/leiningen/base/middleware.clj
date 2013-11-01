(ns leiningen.base.middleware
  (:require [clojure.string :as string]
            [clojure.tools.nrepl.transport :as transport])
  (:use [clojure.tools.nrepl.middleware :only [set-descriptor!]]
        [clojure.tools.nrepl.misc :only [response-for]])
  (:import clojure.tools.nrepl.transport.Transport))



;;
;; Utilities from Ritz
;;

(defmulti transform-value "Transform a value for output" type)

(defmethod transform-value :default [v] v)

(defmethod transform-value java.lang.Boolean
  [v]
  (if v "true" "false"))

(defmethod transform-value java.util.regex.Pattern
  [v]
  (str v))

(defmethod transform-value clojure.lang.PersistentVector
  [v]
  (list* (map transform-value v)))

(defmethod transform-value clojure.lang.LazySeq
  [v]
  (list* (map transform-value v)))

(defmethod transform-value clojure.lang.PersistentHashMap
  [m]
  (list* (mapcat #(vector (name (key %)) (transform-value (val %))) m)))

(defmethod transform-value clojure.lang.PersistentArrayMap
  [m]
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

(defn base-reply
  [{:keys [symbol transport] :as msg}]
  (let [results symbol]
    (transport/send
     transport (response-for msg :value (transform-value results)))
    (transport/send transport (response-for msg :status :done))))

(defn wrap-base [handler]
  (fn [{:keys [op] :as msg}]
    (case op
      "leiningen.base/read" (base-reply msg)
      "leiningen.base/write" (base-reply msg)
      (handler msg))))

(set-descriptor!
 #'wrap-base
 {:handles
  {"leiningen.base/read"
   {:doc "Return a list of forms matching the specified namespace or symbol."
    :requires {"symbol" "The namespace qualified symbol to lookup"}
    :returns {"status" "done"}}
   "leiningen.base/write"
   {:doc "Return a list of forms matching the specified namespace or symbol."
    :requires {"symbol" "The namespace qualified symbol to lookup"}
    :returns {"status" "done"}}}})