(import ../deps/musty/src/musty)


(def sep (if (= :windows (os/which)) "\\" "/"))


(def default-template
  ```
  The default template for generating the API document
  ```
  ````
  # {{project-name}} API

  {{#project-doc}}
  {{&project-doc}}

  {{/project-doc}}
  {{#modules}}
  {{#ns}}
  ## {{ns}}
  {{/ns}}

  {{#items}}{{^first}}, {{/first}}[{{name}}](#{{in-link}}){{/items}}

  {{#doc}}
  {{&doc}}

  {{/doc}}
  {{#items}}
  ## {{name}}

  **{{kind}}** {{#private?}}| **private**{{/private?}} {{#link}}| [source][{{num}}]{{/link}}

  {{#sig}}
  ```janet
  {{&sig}}
  ```
  {{/sig}}

  {{&docstring}}

  {{#link}}
  [{{num}}]: {{link}}
  {{/link}}

  {{/items}}
  {{/modules}}
  ````)


(defn- last-pos
  ```
  Returns the position of the last occurrence of a character or nil
  ```
  [c s]
  (var result nil)
  (var value (get c 0))
  (var i (length s))
  (while (> i 0)
    (when (= value (get s (-- i)))
      (set result i)
      (break)))
  result)


(defn- file-ext
  ```
  Returns the file extension in a path or nil if there is no extension
  ```
  [path]
  (def last-sep (last-pos sep path))
  (def last-dot (last-pos "." path))
  (when (and last-dot
             (or (nil? last-sep)
                 (> last-dot last-sep)))
    # use negative index to count from end
    # increment by an extra 1 for '.'
    (string/slice path last-dot)))


(defn- parent-dir
  ```
  Determines the parent directory containing a file
  ```
  [path]
  (if-let [pos (last-pos sep path)]
    (string/slice path 0 (inc pos))
    (string "." sep)))


(defn- link
  ```
  Creates a link to a specific line in a file
  ```
  [{:file file :line line} project-root link-prefix]
  (if (nil? file)
    nil
    (if (and project-root link-prefix)
      (-> (string/replace project-root link-prefix file)
          (string "#L" line))
      (string file "#L" line))))


(defn- in-link
  ```
  Creates an internal link

  ```
  # Uses the algorithm at https://github.com/gjtorikian/html-pipeline/blob/main/lib/html/pipeline/toc_filter.rb
  [name headings]
  (def key (-> (peg/match ~{:main    (% (any (+ :kept :changed :ignored)))
                            :kept    (<- (+ :w+ (set "_-")))
                            :changed (/ (<- " ") "-")
                            :ignored 1}
                          name)
               (first)))
  (def i (get headings key 0))
  (put headings key (inc i))
  (if (zero? i)
    key
    (string key "-" i)))


(defn- binding->item
  ```
  Prepares the fields for the template
  ```
  [item num first? opts]
  {:num       num
   :first     first?
   :name      (item :name)
   :ns        (item :ns)
   :kind      (string (item :kind))
   :private?  (item :private?)
   :sig       (or (item :sig)
                  (and (not (nil? (item :value)))
                       (string/format "%q" (item :value))))
   :docstring (item :docstring)
   :link      (link item (opts :project-root) (opts :link-prefix))
   :in-link   (in-link (item :name) (opts :headings))})


(defn- bindings->modules
  ```
  Splits an array of bindings into an array of modules.
  ```
  [bindings opts]
  (def modules @[])
  (var curr-ns nil)
  (var items nil)
  (var module nil)
  (var first? false)
  (loop [i :range [0 (length bindings)]
           :let [binding (get bindings i)]]
    (def ns (if (= "" (binding :ns)) false (binding :ns)))
    (if (= curr-ns ns)
      (set first? false)
      (do
        (set curr-ns ns)
        (set items @[])
        (set module @{:ns curr-ns :items items})
        (set first? true)
        (array/push modules module)))
    (if (binding :doc)
      (put module :doc (binding :doc))
      (array/push items (binding->item binding (inc i) first? opts))))
  modules)


(defn- emit-markdown
  ```
  Creates the Markdown-formatted strings
  ```
  [bindings project opts]
  (def template (if (opts :template-file)
                  (slurp (opts :template-file))
                  default-template))
  (put opts :headings @{})
  (musty/render template {:project-name (project :name)
                          :project-doc  (project :doc)
                          :modules      (bindings->modules bindings opts)}))


(defn- source-map
  ```
  Determines the source-map for a given set of metadata
  ```
  [meta]
  (or (meta :source-map)
      (let [ref (-?> (meta :ref) first)]
        (if (= :function (type ref))
          (let [code       (disasm ref)
                file       (code :source)
                [line col] (-> (code :sourcemap) first)]
            [file line col])
          [nil nil nil]))))


(defn- binding-details
  ```
  Creates a table of metadata
  ```
  [name meta maybe-ns]
  (def ns (or (meta :ns) maybe-ns))
  (def value (or (meta :value) (first (meta :ref))))
  (def [file line col] (source-map meta))
  (def kind (cond (meta :macro) :macro
                  (meta :kind) (meta :kind)
                  (type value)))
  (def private? (meta :private))
  (def docs (meta :doc))
  (def [sig docstring] (if (and docs (string/find "\n\n" docs))
                         (string/split "\n\n" docs 0 2)
                         [nil docs]))
  {:name      name
   :ns        ns
   :value     value
   :kind      kind
   :private?  private?
   :sig       sig
   :docstring docstring
   :file      file
   :line      line})


(defn- path->ns
  ```
  Converts a path to a 'namespace'
  ```
  [path project-root defix]
  (string/slice path
                (+ (length project-root) (length defix))
                (when (string/has-suffix? ".janet" path) -7)))


(defn- find-aliases
  ```
  Finds possible aliases

  Bindings that are imported into a namespace and then exported have a `meta`
  length of 1. This can be used as a heuristic to build a table of possible
  aliases that can be used in the `extract-bindings` function. A more robust
  implementation would store the value of the aliased binding and use that later
  to check.
  ```
  [envs project-root defix]
  (def aliases @{})
  (each [path env] (pairs envs)
    (def ns (path->ns path project-root defix))
    (each [name meta] (pairs env)
      (when (one? (length meta))
        (put aliases name ns))))
  aliases)


(defn- document-name?
  ```
  Returns whether a value of the name should be documented

  A value should be documented if it is a bound symbol or it is `:doc`.
  ```
  [name]
  (case name
    :doc true
    (symbol? name)))


(defn- extract-bindings
  ```
  Extracts information about the bindings from the environments
  ```
  [envs opts]
  (def project-root (opts :project-root))
  (def defix (opts :defix))
  (def include-private? (opts :include-private?))
  (def aliases (find-aliases envs project-root defix))
  (defn ns-or-alias [name ns]
    (def alias (aliases name))
    (if (and (not (nil? alias))
             (string/has-prefix? alias ns))
      alias
      ns))
  (def bindings @[])
  (each [path env] (pairs envs)
    (def ns (path->ns path project-root defix))
    (each [name meta] (pairs env)
      (when (and (document-name? name)
                 (or (not (meta :private))
                     include-private?))
        (cond
          (= :doc name)
          (array/push bindings {:ns ns :doc meta})

          (one? (length meta)) # Only aliased bindings should have a meta length of 1
          (->> (binding-details name (table/getproto meta) ns)
               (array/push bindings))

          (->> (ns-or-alias name ns)
               (binding-details name meta)
               (array/push bindings))))))
  (sort-by (fn [binding]
             (string (binding :ns) (binding :name)))
           bindings))


(defn- extract-env
  ```
  Extracts the environment for a file in the project
  ```
  [path]
  (def result @{})
  (def env (if (nil? (file-ext path))
             (require path)
             (dofile path)))
  (unless (nil? env)
    (put env :current-file nil)
    (put env :source nil)
    (put result path env))
  result)


(defn- gather-files
  ```
  Replaces mixture of files and directories with files
  ```
  [paths &opt parent excludes]
  (default parent (string "." sep))
  (default excludes [])
  (mapcat (fn [path]
            (def full-path (string parent path))
            (def kind (os/stat full-path :mode))
            (cond
              (find (fn [x] (string/has-prefix? x full-path)) excludes)
              []

              (= :file kind)
              full-path

              (= :directory kind)
              (gather-files (os/dir full-path) (string full-path sep) excludes)))
          paths))


(defn- validate-project-data
  ```
  Checks that the data in the project.janet file includes everything we need
  ```
  [data]
  (unless (data :project)
    (error "Project file must contain declare-project"))
  (unless (or (data :source) (data :native))
    (error "Project file must contain declare-source or declare-native"))
  data)


(defn- parse-project
  ```
  Parses a project.janet file

  This function returns a table of the values in the project file. The keys are
  the sections but without the leading `declare-` and inserted as keywords.
  ```
  [project-file]
  (def result @{})
  (def p (parser/new))
  (parser/consume p (slurp project-file))
  (while (parser/has-more p)
    (def form (parser/produce p))
    (def head (first form))
    (def tail (tuple/slice form 1))
    (when (string/has-prefix? "declare-" head)
      (def key (keyword (string/slice head 8))) # slice off 'declare-'
      (put result key (struct ;tail))))
  (validate-project-data result))


(defn generate-doc
  ```
  Generates an API document for a project
  ```
  [opts]
  (def project-file (opts :project-file))
  (def project-root (parent-dir project-file))
  (def project-data (parse-project project-file))
  (put opts :project-root project-root)

  (unless (nil? (opts :modpath))
    (put root-env :modpath (opts :modpath)))

  (defn check-build-dir [name]
    (def path (string project-root "build" sep name ".so"))
    (when (= :file (os/stat path :mode))
      path))
  (array/push module/paths [check-build-dir :native])

  (def envs @{})
  (when-let [sources (get-in project-data [:source :source])]
    (def excludes (map (fn [x] (string project-root x)) (opts :exclude)))
    (def paths (gather-files (or (opts :only) sources) project-root excludes))
    (reduce (fn [e p] (merge-into e (extract-env p))) envs paths))

  (when-let [name (get-in project-data [:native :name])]
    (put envs (string project-root name) ((extract-env name) name)))

  (def bindings (extract-bindings envs opts))
  (def document (emit-markdown bindings
                               {:name (get-in project-data [:project :name])
                                :doc  (get-in project-data [:project :doc])}
                               opts))
  (if (opts :echo?)
    (print document)
    (spit (opts :output-file) document)))
