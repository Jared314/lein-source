(ns leiningen.source
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [leiningen.do :as ldo]
            [clj-jgit.porcelain :as jgit]
            [clj-jgit.internal :as i])
  (:import [org.eclipse.jgit.treewalk TreeWalk]
           [java.io FileInputStream FileDescriptor BufferedInputStream]))



;; Task Chaining

(defn- split-args [args]
  (let [[a b] (split-with #(not (.endsWith % ",")) args)
        x (first b)
        x (subs x 0 (dec (count x)))]
    [(concat a (list x)) (rest b)]))



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

(defn- eval-unquoted-forms
  "Inside defproject forms, unquoting (~) allows for arbitrary evaluation."
  [args]
  (walk/walk (fn [item]
               (cond (and (seq? item) (= `unquote (first item))) (eval (second item))
                     :else (let [result (eval-unquoted-forms item)]
                             ;; clojure.walk strips metadata
                             (if-let [m (meta item)]
                               (with-meta result m)
                               result))))
             identity
             args))

(defn read-project-form
  ([form] (read-project-form form [:default]))
  ([form profiles] (read-project-form form profiles main/*cwd*))
  ([[_ project-name version & {:as args}] profiles current-dir]
   (let [current-dir-path (if (string? current-dir)
                            current-dir
                            (.getAbsolutePath (io/as-file current-dir)))]
     (project/make (eval-unquoted-forms args) project-name version current-dir-path))))

(defn read-project-string [args]
  (read-project-form (read-string args)))

(defn read-project-slurp [args]
  (read-project-string (slurp args)))

;; Reads without closing the stream, unlike slurp
(defn read-project-stdin [r]
  (let [sb (StringBuilder.)]
    (loop [c (.read r)]
      (if (neg? c)
        (read-project-string (str sb))
        (do
          (.appendCodePoint sb c)
          (recur (.read r)))))))

(defn read-project-git [args]
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

(defn read-project [f-args]
  (let [sourcetype (string/lower-case (first f-args))
        profiles [:default]]
    (try
      (-> (case sourcetype
            "--file"   (read-project-slurp (second f-args))
            "--string" (read-project-string (second f-args))
            "--git"    (read-project-git (drop 1 f-args))
            "--url"    (read-project-slurp (second f-args))
            "--stdin"  (read-project-stdin *in*))
          project/project-with-profiles
          (project/init-profiles profiles)
          project/init-project)
      (catch Exception e
        (throw (Exception. "Error loading supplied project." e))))))



;; TODO: add project file exception handling
(defn ^:no-project-needed ^:higher-order
  source
  "A Leiningen plugin to pull project configuration from different locations."
  [project & args]
  (let [[f-args other-args] (split-args args)
        realproject (read-project f-args)]
    (when (:min-lein-version realproject) (verify-min-version realproject))
    (apply (partial ldo/do realproject) other-args)))
