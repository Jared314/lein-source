(ns leiningen.source.analyzer
  (:require [clojure.java.io :as io])
  (:import [java.io PushbackReader StringReader]))

(defn lazy-read
  ([rdr] (let [eof (Object.)] (lazy-read rdr (read rdr false eof) eof)))
  ([rdr data eof]
   (when (not= eof data)
     (cons data (lazy-seq (lazy-read rdr (read rdr false eof) eof))))))

(defn read-all [in]
  (with-open [rdr (clojure.lang.LineNumberingPushbackReader. (io/reader in))]
    (doall (lazy-read rdr))))


(defn parse-ns-form [[op name & references]]
  (let [process-reference
        (fn [[kname & args]]
          `(~(symbol "clojure.core" (clojure.core/name kname))
             ~@(map #(list 'quote %) args)))
        docstring  (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        metadata   (when (map? (first references)) (first references))
        references (if metadata (next references) references)
        name (if metadata
               (vary-meta name merge metadata)
               name)
        gen-class-clause (first (filter #(= :gen-class (first %)) references))
        gen-class-call
        (when gen-class-clause
          (list* `gen-class :name (namespace-munge name) :impl-ns name :main true (next gen-class-clause)))
        references (remove #(= :gen-class (first %)) references)
        reference-map (reduce #(update-in %1 [(first %2)] conj (second %2)) {} references)
        ;ns-effect (clojure.core/in-ns name)
        ]
    {:clj/op op
     :clj/docstring docstring
     :clj/def name
     :clj/metadata metadata
     :clj/ns-gen-class gen-class-clause
     :clj/ns-references reference-map}))


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
