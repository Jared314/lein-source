(ns leiningen.base
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [leiningen.do :as ldo]
            [clj-jgit.porcelain :as jgit]
            [clj-jgit.internal :as i])
  (:import [org.eclipse.jgit.treewalk TreeWalk]
           [java.net URL Proxy URLStreamHandlerFactory URLStreamHandler URLConnection]
           [java.io File PushbackReader StringReader]))


(defn update-values [f m]
  (into {} (for [[k v] m] [k (f k v)])))

(defn index-coll
  ([f coll] (index-coll f {} coll))
  ([f base-coll coll]
   (into base-coll (for [x coll] [(f x) x]))))

(defn namespace->path
  ([n] (namespace->path n "clj"))
  ([n ext] (-> n namespace-munge (clojure.string/replace \. \/) (str "." ext))))

(defprotocol TextBlockStore
  (exists? [this n] nil)
  (read-ns [this n] nil)
  (generate-ns-writes [this n v] nil))

(defrecord FileStorage [basepath]
  TextBlockStore
  (exists? [this n]
           (let [f (io/file basepath (namespace->path n))]
             (and (.exists f)
                  (.isFile f))))
  (read-ns [this n] (slurp (io/file basepath (namespace->path n))))
  (generate-ns-writes [this n v]
                      (let [f (io/file basepath (namespace->path n))]
                        (if (exists? this n)
                          [#(spit f v)]
                          [#(.mkdirs (.getParentFile f))
                           #(spit f v)]))))

;; Task Chaining

(defn- split-args [args]
  (let [{a 0 b 1 :or {a '() b '()}} (split-with #(not (.endsWith % ",")) args)
        x (first b)
        x (when x (subs x 0 (dec (count x))))]
    [(concat a (when x (list x))) (rest b)]))

;;;;;;;;;;;;;;;;;;;
;; Form Reading
;;;;;;;;;;;;;;;;;;;

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
  (let [ns? #(and (coll? %) (= 'ns (first %)))]
  (with-open [r (PushbackReader. (io/reader src))]
    (second (doall
             (reduce #(let [[current-ns data] %1
                            current-ns (if (ns? %2) (second %2) current-ns)]
                        [current-ns
                         (conj data (analyze-form current-ns %2))])
                     [nil []]
                     (lazy-read r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File-based "DB"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-declare-form [nsname data]
  (let [defed-syms (sort (map #(-> % :clj/def symbol name)
                              (filter :clj/def data)))]
    {:clj/ns nsname
     :clj/op "declare"
     :clj/def defed-syms
     :clj/form (conj (map symbol defed-syms) 'declare)}))

(defn sort-forms [coll]
  (sort-by #(case (:clj/op %) "ns" "0" "declare" "1" (:clj/def %)) coll))

(defn format-forms [current-ns item]
  (sort-forms (conj item (generate-declare-form current-ns item))))

(defn format-namespaces [data]
  (update-values (fn [k v]
                   (clojure.string/join "\n\n" (map :clj/form (format-forms k v))))
                 data))

(defn merge-with-existing-files [storage m]
  (let [nss (filter (partial exists? storage) (keys m))
        joined-forms (string/join "\n\n" (map (partial read-ns storage) nss))
        existing-m (when-not (empty? joined-forms)
                     (group-by :clj/ns (analyze (StringReader. joined-forms))))]
    (if (empty? existing-m)
      m
      (merge-with #(vals (merge (index-coll :clj/def %1)
                                (index-coll :clj/def %2)))
                  existing-m
                  m))))

(defn generate-writes [storage m]
  (->> m
       (group-by :clj/ns)
       (merge-with-existing-files storage)
       format-namespaces
       (reduce #(let [[k v] %2]
                  (into %1 (generate-ns-writes storage k v))) [])))


;;
;; echo "(ns stuff.core) (defn thing [] true)" | lein base .
;;
(defn
  base
  "A Leiningen plugin to read, write, and run forms in a backend."
  [project & args]
  (let [[f-args other-args] (split-args args)
        targetpath (.getCanonicalFile (io/as-file (first f-args)))
        storage (FileStorage. (io/file targetpath "src"))]
    (->> *in*
         analyze
         (generate-writes storage)
         (map #(%))
         dorun)
    (when (first other-args) (apply (partial ldo/do project) other-args))))
