(ns leiningen.source
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [leiningen.core.main :as main]
            [leiningen.core.user :as user]
            [leiningen.core.eval :as leval]
            [leiningen.core.project :as project]
            [leiningen.repl :as repl]
            [leiningen.do :as ldo]
            [leiningen.source.storage-provider :refer :all]
            [leiningen.source.storage-provider.textblockstore :refer [->FileStorage]]
            [leiningen.source.analyzer :as a]
            [clojure.tools.nrepl.server :as server]
            [leiningen.source.middleware :as middleware])
  (:import [org.eclipse.jgit.treewalk TreeWalk]
           [java.net URL Proxy URLStreamHandlerFactory URLStreamHandler URLConnection]
           [java.io File PushbackReader StringReader]))


(defn print-dup-str [x]
  (binding [*print-dup* true]
    (prn-str x)))

(defn print-with [out x]
  (binding [*out* out]
    (print x)
    (flush)))

;;
;; Because leiningen.repl makes them private
;;

(defn- init-requires [{{:keys [nrepl-middleware nrepl-handler]} :repl-options
                       :as project} & nses]
  (let [defaults '[clojure.tools.nrepl.server complete.core]
        nrepl-syms (->> (cons nrepl-handler nrepl-middleware)
                        (filter symbol?)
                        (map namespace)
                        (remove nil?)
                        (map symbol))]
    (for [n (concat defaults nrepl-syms nses)]
      (list 'quote n))))

(defn- wrap-init-ns [project]
  (if-let [init-ns (repl/init-ns project)]
    ;; set-descriptor! currently nREPL only accepts a var
    `(with-local-vars
         [wrap-init-ns#
          (fn [h#]
            ;; this needs to be a var, since it's in the nREPL session
            (with-local-vars [init-ns-sentinel# nil]
              (fn [{:keys [~'session] :as msg#}]
                (when-not (@~'session init-ns-sentinel#)
                  (swap! ~'session assoc
                         (var *ns*)
                         (try (require '~init-ns) (create-ns '~init-ns)
                              (catch Throwable t# (create-ns '~'user)))
                         init-ns-sentinel# true))
                (h# msg#))))]
       (doto wrap-init-ns#
         (clojure.tools.nrepl.middleware/set-descriptor!
          {:requires #{(var clojure.tools.nrepl.middleware.session/session)}
           :expects #{"eval"}})
         (alter-var-root (constantly @wrap-init-ns#))))))

(defn- handler-for [{{:keys [nrepl-middleware nrepl-handler]} :repl-options,
                     :as project}]
  (when (and nrepl-middleware nrepl-handler)
    (main/abort "Can only use one of" :nrepl-handler "or" :nrepl-middleware))
  (let [nrepl-middleware (remove nil? (concat [(wrap-init-ns project)]
                                              nrepl-middleware))]
    (or nrepl-handler
        `(server/default-handler
           ~@(map #(if (symbol? %) (list 'var %) %) nrepl-middleware)))))

(defn- server-forms [project cfg ack-port start-msg?]
  [`(let [server# (server/start-server
                   :bind ~(:host cfg) :port ~(:port cfg)
                   :ack-port ~ack-port
                   :handler ~(handler-for project))
          port# (:port server#)
          repl-port-file# (apply io/file ~(if (:root project)
                                            [(:root project) ".nrepl-port"]
                                            [(user/leiningen-home) "repl-port"]))
          legacy-repl-port# (if (.exists (io/file ~(:target-path project)))
                              (io/file ~(:target-path project) "repl-port"))]
      (when ~start-msg?
        (println "nREPL server started on port" port# "on host" ~(:host cfg)))
      (spit (doto repl-port-file# .deleteOnExit) port#)
      (when legacy-repl-port#
        (spit (doto legacy-repl-port# .deleteOnExit) port#))
      @(promise))
   ;; TODO: remove in favour of :injections in the :repl profile
   `(do ~(when-let [init-ns (repl/init-ns project)]
           `(try (doto '~init-ns require in-ns)
                 (catch Exception e# (println e#) (ns ~init-ns))))
        ~@(for [n (init-requires project)]
            `(try (require ~n)
                  (catch Throwable t#
                    (println "Error loading" (str ~n ":")
                             (or (.getMessage t#) (type t#))))))
        ~(-> project :repl-options :init))])



;; Task Chaining

(defn- split-args [args]
  (let [{a 0 b 1 :or {a '() b '()}} (split-with #(not (.endsWith % ",")) args)
        x (first b)
        x (when x (subs x 0 (dec (count x))))]
    [(concat a (when x (list x))) (rest b)]))



;; Actions

;; Handle Stream

(defn write-forms [backend in]
  (->> in
       a/analyze
       (store backend)
       (map #(%))
       dorun))

(defn get-forms [backend in out]
  (let [q (string/trim (slurp in))]
    (->> (query backend q)
         (map #(print-dup-str (:clj/form %)))
         (clojure.string/join "\n")
         (print-with out))))

(defn read-op? [^java.io.PushbackReader s]
  (let [c (.read s)]
    (.unread s c)
    (not= \( (char c))))

(defn handle-stream [project args]
  (let [source-path (first (:source-paths project))
        backend (->FileStorageProvider (->FileStorage source-path)
                                      a/analyze)]
    (if (read-op? *in*)
      (get-forms backend *in* *out*)
      (write-forms backend *in*))))


;; Handle nREPL

(defn build-resource-inject [v]
  (let [f (io/resource (:file (meta v)))]
    (a/read-all f)))

(defn build-backend-inject [source-path]
    (list 'reset! 'leiningen.source.middleware/backend
          (list '->FileStorageProvider
                (list '->FileStorage source-path)
                'leiningen.source.analyzer/analyze)))

(defn build-version-inject []
  (let [fs (.getResources (.getContextClassLoader (Thread/currentThread))
                          "project.clj")
        f (first (filter #(not= -1 (.indexOf (str %) "lein-source")) (enumeration-seq fs)))
        v (when f (nth (read-string (slurp f)) 2))]
    (list 'reset! 'leiningen.source.middleware/version v)))

(defn handle-repl [project opts]
  (let [backend (build-backend-inject (first (:source-paths project)))
        version (build-version-inject)
        injects (concat (build-resource-inject #'a/analyze)
                        (build-resource-inject #'leiningen.source.storage-provider.textblockstore/->FileStorage)
                        (build-resource-inject #'leiningen.source.storage-provider/->FileStorageProvider)
                        (build-resource-inject #'middleware/wrap-source)
                        [backend version])
        p (update-in project
                     [:repl-options :nrepl-middleware]
                     #(into [#'middleware/wrap-source] %))
        p (update-in p [:injections] concat injects)
        opts (nnext opts)
        cfg {:host (or (repl/opt-host opts) (repl/repl-host project))
             :port (or (repl/opt-port opts) (repl/repl-port project))}]
    ;; Duplicate of leiningen.repl :headless because
    ;; a supplied project map to leiningen.repl/repl
    ;; is overwritten with defaults pulled from the environment.
    (apply leval/eval-in-project
           p
           (server-forms p
                     cfg
                     (repl/ack-port project)
                     true))))

(defn parse-args [project args]
  (let [opt (string/lower-case (string/trim (second args)))]
    (case opt
      "--nrepl" (handle-repl project args)
      (handle-stream project args))))

;;
;; echo "(ns stuff.core) (defn thing [] true)" | lein source .
;; lein source . --nrepl :port 12345
;; (require '[clojure.tools.nrepl :as repl])
;; (with-open [conn (repl/connect :port 12345)] (-> (repl/client conn 1000) (repl/message { :op "leiningen.source/version" }) repl/response-values))
;;
(defn
  source
  "A Leiningen plugin to read, write, and run forms in a backend."
  [project & args]
  (let [[f-args other-args] (split-args args)]
    (parse-args project f-args)
    (when (first other-args) (apply (partial ldo/do project) other-args))))

