(ns leiningen.base
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [leiningen.do :as ldo]
            [clj-jgit.porcelain :as jgit]
            [clj-jgit.internal :as i]
            [leiningen.base.storage-provider :refer :all])
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



;; Form Reading

(defn lazy-read
  ([rdr] (let [eof (Object.)] (lazy-read rdr (read rdr false eof) eof)))
  ([rdr data eof]
   (when (not= eof data)
     (cons data (lazy-seq (lazy-read rdr (read rdr false eof) eof))))))

(defn read-all [in]
  (with-open [rdr (clojure.lang.LineNumberingPushbackReader. (io/reader in))]
    (doall (lazy-read rdr))))

(defn analyze-form [current-ns x]
  (let [op (first x)
        ns? (= op 'ns)
        defing? (and current-ns
                     (symbol? op)
                     (.startsWith (name op) "def"))
        nsname (str (if ns? (second x) current-ns))
        naming (let [nsym (second x)]
                 (cond
                  ns? (str nsym)
                  defing? (if (namespace nsym)
                            (str nsym)
                            (str (symbol (name current-ns) (name nsym))))))]
    (cond-> {:clj/ns nsname
             :clj/op (str op)
             :clj/form x}
            defing? (assoc :clj/def naming))))

(defn analyze [src]
  (let [src (if (string? src) (StringReader. src) src)
        ns? #(and (coll? %) (= 'ns (first %)))]
  (with-open [r (PushbackReader. (io/reader src))]
    (second (doall
             (reduce #(let [[current-ns data] %1
                            current-ns (if (ns? %2) (second %2) current-ns)]
                        [current-ns
                         (conj data (analyze-form current-ns %2))])
                     [nil []]
                     (lazy-read r)))))))



;; Task Chaining

(defn- split-args [args]
  (let [{a 0 b 1 :or {a '() b '()}} (split-with #(not (.endsWith % ",")) args)
        x (first b)
        x (when x (subs x 0 (dec (count x))))]
    [(concat a (when x (list x))) (rest b)]))



;; Actions

(defn write-forms [backend in]
  (->> in
       analyze
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
                                      analyze)
        query (read-op? *in*)]
    (if query
      (get-forms backend *in* *out*)
      (write-forms backend *in*))
    (when (first other-args) (apply (partial ldo/do project) other-args))))

