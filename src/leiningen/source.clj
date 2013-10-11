(ns leiningen.source
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [leiningen.do :as ldo]
            [clj-http.client :as http]
            [clj-jgit.porcelain :as jgit]
            [clj-jgit.internal :as i])
  (:import [org.eclipse.jgit.treewalk TreeWalk]))

;; Task Chaining

(defn- normalize-args [args]
  (let [loc (inc (count (take-while #(not (.endsWith % ",")) args)))
        [taskargs otherargs] (split-at loc args)]
    [(map #(string/replace % #",$" "") taskargs)
     otherargs]))



;; Initialization

(def ^:private min-version-warning
  "*** Warning: This project requires Leiningen %s, but you have %s ***

Get the latest version of Leiningen at http://leiningen.org or by executing
\"lein upgrade\".")

(defn- verify-min-version [{:keys [min-lein-version]}]
  (let [lv (main/leiningen-version)]
    (when-not (main/version-satisfies? lv min-lein-version)
      (main/info (format min-version-warning min-lein-version lv)))))



;; Read Project Form

(defn- read-project-file [args]
  (let [f (io/file args)]
    (when (.exists f)
      (project/read (.getCanonicalPath  f)))))

;;TODO: Using eval and read-string because load-string caused NullReferenceException
;;      in defproject call
;;      Replace eval usage with something else
(defn- read-project-string [args]
  (let [profiles [:default]]
    (locking read-project-string
      (binding [*ns* (the-ns 'leiningen.core.project)
                *file* (.getCanonicalPath (io/file "non-existent-file"))]
        (try (eval (read-string args))
          (catch Exception e
            (throw (Exception. (format "Error loading supplied string.") e))))
        (let [project (resolve 'leiningen.core.project/project)]
          (when-not project
            (throw (Exception. (format "Supplied string must define project map"))))
          (ns-unmap 'leiningen.core.project 'project)
          (project/init-profiles (project/project-with-profiles @project) profiles))))))

(defn- read-project-stream [instream]
  (let [data (slurp instream)]
    (read-project-string data)))

(defn- read-project-git [args]
  (let [repo-path (nth args 0)
        commitid (nth args 1 "HEAD")
        objectpath (nth args 2 "project.clj")]
    (jgit/with-repo repo-path
                    (let [reader (.newObjectReader (.getRepository repo))]
                      (when-let [o (i/resolve-object commitid repo)]
                        (-> (TreeWalk/forPath reader
                                              objectpath
                                              (into-array [(.parseTree rev-walk o)]))
                            (.getObjectId 0)
                            (->> (.open reader))
                            (.getBytes)
                            (String. "utf-8")
                            (read-project-string)))))))

(defn- read-project-url [args]
  (let [result (http/get args)]
    (when (= 200 (:status result))
      (read-project-string (:body result)))))

(defn read-project [f-args]
  (let [sourcetype (string/lower-case (first f-args))]
    (project/init-project
     (case sourcetype
       "--file"   (read-project-file (second f-args))
       "--string" (read-project-string (second f-args))
       "--git"    (read-project-git (drop 1 f-args))
       "--url"    (read-project-url (second f-args))
       "--stdin"  (read-project-stream *in*)))))



;; TODO: add project file exception handling
(defn ^:no-project-needed ^:higher-order
  source
  "A Leiningen plugin to pull project configuration from different locations."
  [project & args]
  (let [[f-args other-args] (normalize-args args)
        realproject (read-project f-args)]
    (when (:min-lein-version realproject) (verify-min-version realproject))
    (apply (partial ldo/do realproject) other-args)))
