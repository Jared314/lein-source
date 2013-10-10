(ns leiningen.source
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]))

;; Task Chaining

(defn- conj-to-last [coll x]
  (update-in coll [(dec (count coll))] conj x))

(defn ^:internal group-args
  ([args] (reduce group-args [[]] args))
  ([groups arg]
   (if (.endsWith arg ",")
     (-> groups
         (conj-to-last (subs arg 0 (dec (count arg))))
         (conj []))
     (conj-to-last groups arg))))



;; Initialization

(defn ^:internal version-satisfies? [v1 v2]
  (let [v1 (map #(Integer. %) (re-seq #"\d+" (first (string/split v1 #"-" 2))))
        v2 (map #(Integer. %) (re-seq #"\d+" (first (string/split v2 #"-" 2))))]
    (loop [versions (map vector v1 v2)
           [seg1 seg2] (first versions)]
      (cond (empty? versions) true
            (= seg1 seg2) (recur (rest versions) (first (rest versions)))
            (> seg1 seg2) true
            (< seg1 seg2) false))))

(def ^:private min-version-warning
  "*** Warning: This project requires Leiningen %s, but you have %s ***

Get the latest version of Leiningen at http://leiningen.org or by executing
\"lein upgrade\".")

(defn- verify-min-version [{:keys [min-lein-version]}]
  (let [lv (main/leiningen-version)]
    (when-not (version-satisfies? lv min-lein-version)
      (main/info (format min-version-warning min-lein-version lv)))))



;; Read Project Form

(defn- read-project-file [args]
  (let [f (io/file args)]
    (when (.exists f)
      (project/read f))))

(defn- read-project-string [args]
  (let [profiles [:default]]
    (locking read-project-string
      (binding [*ns* (find-ns 'leiningen.core.project)]
        (try (load-string args)
          (catch Exception e
            (throw (Exception. (format "Error loading supplied string.") e))))
        (let [project (resolve 'leiningen.core.project/project)]
          (when-not project
            (throw (Exception. (format "Supplied string must define project map"))))
          (ns-unmap 'leiningen.core.project 'project)
          (project/init-profiles (project/project-with-profiles @project) profiles))))))

(defn- read-project-git [args]
  (throw (UnsupportedOperationException.)))

(defn- read-project-url [args]
  (throw (UnsupportedOperationException.)))

(defn read-project [f f-args]
  (let [valuetype (string/lower-case f)]
    (project/init-project
     (case valuetype
       "-projectfile"   (read-project-file f-args)
       "-string" (read-project-string f-args)
       "-git"    (read-project-git f-args)
       "-url"    (read-project-url f-args)))))



;; TODO: add project file exception handling
(defn ^:no-project-needed ^:higher-order
  source
  "I don't do a lot."
  [project f f-args & args]
  (let [realproject (read-project f f-args)]
    (when (:min-lein-version realproject) (verify-min-version realproject))
    (doseq [arg-group (group-args args)]
      (main/resolve-and-apply realproject arg-group))))
