(ns leiningen.base
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [leiningen.do :as ldo]
            [leiningen.base.storage-provider :refer :all]
            [leiningen.base.analyzer :as a])
  (:import [org.eclipse.jgit.treewalk TreeWalk]
           [java.net URL Proxy URLStreamHandlerFactory URLStreamHandler URLConnection]
           [java.io File PushbackReader StringReader]
           [leiningen.base.storage_provider.textblockstore FileStorage]
           [leiningen.base.storage_provider FileStorageProvider]))


(defn print-dup-str [x]
  (binding [*print-dup* true]
    (prn-str x)))

(defn print-with [out x]
  (binding [*out* out]
    (print x)
    (flush)))


;; Task Chaining

(defn- split-args [args]
  (let [{a 0 b 1 :or {a '() b '()}} (split-with #(not (.endsWith % ",")) args)
        x (first b)
        x (when x (subs x 0 (dec (count x))))]
    [(concat a (when x (list x))) (rest b)]))



;; Actions

(defn write-forms [backend in]
  (->> in
       a/analyze
       (store backend)
       (map #(%))
       dorun))

(defn get-forms [backend in out]
  (let [q (string/trim (slurp in))]
    (->> (query backend q)
         (map #(print-dup-str (:clj/form %)))
         (clojure.string/join "\n")
         (print-with out))))

(defn read-op? [^java.io.PushbackReader s]
  (let [c (.read s)]
    (.unread s c)
    (not= \( (char c))))



;;
;; echo "(ns stuff.core) (defn thing [] true)" | lein base .
;;
(defn
  base
  "A Leiningen plugin to read, write, and run forms in a backend."
  [project & args]
  (let [[f-args other-args] (split-args args)
        targetpath (.getCanonicalFile (io/as-file (first f-args)))
        backend (FileStorageProvider. (FileStorage. (io/file targetpath "src"))
                                      a/analyze)
        query (read-op? *in*)]
    (if query
      (get-forms backend *in* *out*)
      (write-forms backend *in*))
    (when (first other-args) (apply (partial ldo/do project) other-args))))

