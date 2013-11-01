(ns leiningen.base.analyzer
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.io PushbackReader StringReader]))



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
