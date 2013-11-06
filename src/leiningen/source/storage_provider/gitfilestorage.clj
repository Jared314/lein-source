(ns leiningen.source.storage-provider.gitblockstore
  (:require [leiningen.source.storage-provider.textblockstore :as tbs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clj-jgit.porcelain :as jgit]
            [clj-jgit.internal :as i]))


(defn get-git-object
  ([repo-path objectpath] (get-git-object-stream repo-path "HEAD" objectpath))
  ([repo-path commitid objectpath]
   (jgit/with-repo repo-path
                   (try
                     (let [reader (.newObjectReader (.getRepository repo))]
                       (when-let [o (i/resolve-object commitid repo)]
                         (-> (TreeWalk/forPath reader
                                               objectpath
                                               (into-array [(.parseTree rev-walk o)]))
                             (.getObjectId 0)
                             (->> (.open reader))
                             (.getBytes))))
                     (finally (when reader (.release reader)))))))

(defrecord GitFileStorage [basepath commitid]
  TextBlockStore
  (exists? [this n]
           (let [f (io/file basepath (tbs/namespace->path n))]
             (and (.exists f)
                  (.isFile f))))
  (read-ns [this n]
           (String. (get-git-object basepath commitid (tbs/namespace->path n))
                    "utf-8"))
  (generate-ns-writes [this n v] []))
