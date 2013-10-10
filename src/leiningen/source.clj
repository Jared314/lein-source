(ns leiningen.source
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [leiningen.do :as ldo]
            [clj-http.client :as http]))

;; Task Chaining

(defn- normalize-args [args]
  (string/replace args #",$" ""))



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

(defn- read-project-stdin [args]
  (let [data (slurp (System/in))]
    (read-project-string data)))

(defn- read-project-git [args]
  (throw (UnsupportedOperationException.)))

(defn- read-project-url [args]
  (let [result (http/get args)]
    (when (= 200 (:status result))
      (read-project-string (:body result)))))

(defn read-project [f f-args]
  (let [sourcetype (string/lower-case f)]
    (project/init-project
     (case sourcetype
       "--file"   (read-project-file f-args)
       "--string" (read-project-string f-args)
       "--git"    (read-project-git f-args)
       "--url"    (read-project-url f-args)
       "--stdin"  (read-project-stdin f-args)))))



;; TODO: add project file exception handling
(defn ^:no-project-needed ^:higher-order
  source
  "A Leiningen plugin to pull project configuration from different locations."
  [project f f-args & args]
  (let [f-args (normalize-args f-args)
        realproject (read-project f f-args)]
    (when (:min-lein-version realproject) (verify-min-version realproject))
    (apply (partial ldo/do realproject) args)))
