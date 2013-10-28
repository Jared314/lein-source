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
           [java.io File PushbackReader]))


(defn update-values [f m]
  (into {} (for [[k v] m] [k (f k v)])))

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

(defn generate-writes [basepath [k v]]
  (let [p (-> k namespace-munge (clojure.string/replace \. \/) (str ".clj"))
        f (io/file basepath p)]
    [#(.mkdirs (.getParentFile f))
     #(spit f v)]))

(defn generate-ns-writes [basepath m]
  (->> m
       (group-by :clj/ns)
       format-namespaces
       (reduce #(into %1 (generate-writes basepath %2)) [])))


;;
;; echo "(ns stuff.core) (defn thing [] true)" | lein base .
;;
(defn
  base
  "A Leiningen plugin to read, write, and run forms in a backend."
  [project & args]
  (let [targetpath (.getCanonicalFile (io/as-file (first args)))]
    (->> *in*
         analyze
         (generate-ns-writes (io/file targetpath "src"))
         (map #(%))
         dorun)
    (println "Done.")))
