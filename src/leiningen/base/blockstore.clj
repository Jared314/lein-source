(ns leiningen.base.blockstore
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defprotocol TextBlockStore
  (exists? [this n] nil)
  (read-ns [this n] nil)
  (generate-ns-writes [this n v] nil))


(defn- namespace->path
  ([n] (namespace->path n "clj"))
  ([n ext] (-> n namespace-munge (string/replace \. \/) (str "." ext))))

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
