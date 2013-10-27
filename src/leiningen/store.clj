(ns leiningen.store
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

;;;;;;;;;;;;;;;;;;;
;; Form Reading
;;;;;;;;;;;;;;;;;;;

(defn lazy-read
  ([rdr] (let [eof (Object.)] (lazy-read rdr (read rdr false eof) eof)))
  ([rdr data eof]
   (when (not= eof data)
     (cons data (lazy-seq (lazy-read rdr (read rdr false eof) eof))))))

(defn analyze-form [current-ns x]
  (let [op (first x)
        ns? (= op 'ns)
        defing? (and current-ns
                     (symbol? op)
                     (.startsWith (name op) "def"))
        naming (let [nsym (second x)]
                 (cond
                  ns? (str nsym)
                  defing? (if (namespace nsym)
                            (str nsym)
                            (str (symbol (name current-ns) (name nsym))))))]
    (cond-> {:clj/op (str op)
             :clj/form x}
            ns? (assoc :clj/ns naming)
            defing? (assoc :clj/def naming))))

(defn analyze [src]
  (let [ns? #(and (coll? %) (= 'ns (first %)))]
    (with-open [r (PushbackReader. (StringReader. src))]
      (second (doall
               (reduce #(let [[current-ns data] %1
                              current-ns (if (ns? %2) (second %2) current-ns)]
                          [current-ns
                           (conj data (analyze-form current-ns %2))])
                       [nil []]
                       (lazy-read r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write-only File-based "DB"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Leaf [nss declares forms])

(def default-layout {"src" {}
                     "test" {}
                     "project" (map->Leaf {:forms ["(defproject tester1 \"0.1.0-SNAPSHOT\" :dependencies [[org.clojure/clojure \"1.5.1\"]])"]})})

(defn parse-value [value]
  value)

(defn compare-value [v1 v2]
  (compare (parse-value v1) (parse-value v2)))

(defn insert! [data item]
  (if (nil? data)
    (Leaf. [(:ns item)] [(:name item)] (sorted-set-by compare-value (:source item)))
    (let [nss (conj (:nss data) (:ns item))
          declares (conj (:declares data) (:name item))
          forms (conj (:forms data) (:source item))]
      (assoc data :nss nss :declares declares :forms forms))))

(defn parse-ns-name [value]
  (string/split value #"\."))

(defn parse [base key namespace value]
  (update-in base
             (concat [key] (parse-ns-name namespace))
             #(insert! % value)))

(defn layout-data [data]
  (if (not (instance? Leaf data))
    (list data)
    (lazy-cat (map #(:source %) (:nss data)) ; ns
              (map #(str "(declare " % ")") (:declares data)) ; declare
              (:forms data))))

(defn format-name [value] (string/replace value "-" "_"))

(defn write! [base-path data]
  (let [keys (keys data)
        leaves (filter #(instance? Leaf (data %)) keys)
        nodes (filter #(not (instance? Leaf (data %))) keys)
        base-file (io/as-file base-path)]
    (dorun (map #(spit (str base-path File/separator (format-name %) ".clj")
                       (string/join "\n" (layout-data (data %))))
                leaves))
    (for [n nodes]
      (let [new-path (str base-path File/separator (format-name n))
            new-file (io/as-file new-path)]
        (when-not (.exists new-file) (.mkdir new-file))
        (write! new-path (data n))))))



(def result
     (reduce #(parse %1 "src" (:name (:ns %2)) %2)
             default-layout
             data))


;; echo "(ns stuff) (defn thing [] true)" | lein store --stdin

;(def data
;  (java.io.StringReader. "(ns stuff) (defn thing [] true)"))


(defn
  store
  "A Leiningen plugin to store forms to a backend."
  [project & args]
  (let [targetpath (io/as-file (first args))]
    (read-all *in*)
    ))
