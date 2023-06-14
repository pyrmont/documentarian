(import argy-bargy :as argy)
(import musty)


(def- sep (if (= :windows (os/which)) "\\" "/"))


(def- config
  {:rules ["--defix"       {:kind  :single
                            :short "d"
                            :proxy "prefix"
                            :help  "Remove prefix from all namespaces."}
           "--echo"        {:kind  :flag
                            :short "e"
                            :help  "Output to stdout rather than output file."}
           "--exclude"     {:kind  :multi
                            :short "x"
                            :proxy "path"
                            :help  "Exclude bindings in <path> from the output."}
           "--input"       {:kind  :single
                            :short "i"
                            :proxy "path"
                            :help  "Use <path> as project file."}
           "--link-parent" {:kind  :single
                            :short "l"
                            :proxy "url"
                            :help  "Replace project root with <url> in source code links."}
           "--output"      {:kind  :single
                            :short "o"
                            :proxy "path"
                            :help  "Use <path> as output file."}
           "--private"     {:kind  :flag
                            :short "p"
                            :help  "Include private values in output."}
           "--template"    {:kind  :single
                            :short "t"
                            :proxy "path"
                            :help  "Use template at <path> for output."}]
   :info {:about "A document generation tool for Janet projects."}})


(def- default-template
  ````
  # {{project-name}} API

  {{#project-doc}}
  {{&project-doc}}

  {{/project-doc}}
  {{#modules}}
  ## {{ns}}

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
  Return the position of the last occurrence of a character or nil
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
  Return the file extension in a path or nil if there is no extension
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
  Determine the parent directory containing a file
  ```
  [path]
  (if (or (string/has-prefix? (string "." sep) path)
          (string/has-prefix? (string ".." sep) path)
          (string/has-prefix? sep path))
    (string/slice path 0 (inc (last-pos sep path)))
    (string "." sep)))


(defn- link
  ```
  Create a link to a specific line in a file
  ```
  [{:file file :line line} project-root link-parent]
  (if (nil? file)
    nil
    (if (and project-root link-parent)
      (-> (string/replace project-root link-parent file)
          (string "#L" line))
      (string file "#L" line))))


(def- headings @{})
(defn- in-link
  ```
  Create an internal link

  ```
  # Uses the algorithm at https://github.com/gjtorikian/html-pipeline/blob/main/lib/html/pipeline/toc_filter.rb
  [name]
  (def key (-> (peg/match ~{:main      (% (any (+ :kept :changed :ignored)))
                            :kept      (<- (+ :w+ (set "_-")))
                            :changed   (/ (<- " ") "-")
                            :ignored   1}
                          name)
               (first)))
  (def i (or (get headings key) 0))
  (put headings key (inc i))
  (if (zero? i)
    key
    (string key "-" i)))


(defn binding->item
  ```
  Prepare the fields for the template
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
   :link      (link item (opts :project-root) (opts :link-parent))
   :in-link   (in-link (item :name))})


(defn- bindings->modules
  ```
  Split an array of bindings into an array of modules.
  ```
  [bindings opts]
  (def modules @[])
  (var curr-ns nil)
  (var items nil)
  (var module nil)
  (var first? false)
  (loop [i :range [0 (length bindings)]
           :let [binding (get bindings i)]]
    (if (= curr-ns (binding :ns))
      (set first? false)
      (do
        (set curr-ns (binding :ns))
        (set items @[])
        (set module @{:ns curr-ns :items items})
        (set first? true)
        (array/push modules module)))
    (if (binding :doc)
      (put module :doc (binding :doc))
      (array/push items (binding->item binding (inc i) first? opts))))
  modules)


(defn emit-markdown
  ```
  Create the Markdown-formatted strings
  ```
  [bindings project opts]
  (def template (if (opts :template-file)
                  (slurp (opts :template-file))
                  default-template))
  (musty/render template {:project-name (project :name)
                          :project-doc  (project :doc)
                          :modules      (bindings->modules bindings opts)}))


(defn- source-map
  ```
  Determine the source-map for a given set of metadata
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
  Create a table of metadata
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
  Convert a path to a 'namespace'
  ```
  [path project-root defix]
  (string/slice path
                (+ (length project-root) (length defix))
                (when (string/has-suffix? ".janet" path) -7)))


(defn- find-aliases
  ```
  Find possible aliases

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


(defn document-name?
  ```
  Given some binding in an environment, determine whether it's eligible for
  rendering as documentation.

  Eligible bindings are:
  - any bound symbol
  - `:doc`
  ```
  [name]
  (case name
    :doc true
    (symbol? name)))


(defn extract-bindings
  ```
  Extract information about the bindings from the environments
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
          nil

          (->> (ns-or-alias name ns)
               (binding-details name meta)
               (array/push bindings))))))
  (sort-by (fn [binding]
             (string (binding :ns) (binding :name)))
           bindings))


(defn extract-env
  ```
  Extract the environment for a file in the project
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


(defn gather-files
  ```
  Replace mixture of files and directories with files
  ```
  [paths &opt parent exclusions]
  (default parent (string "." sep))
  (default exclusions [])
  (mapcat (fn [path]
            (def full-path (string parent path))
            (def kind (os/stat full-path :mode))
            (cond
              (find (fn [x] (string/has-prefix? x full-path)) exclusions)
              []

              (= :file kind)
              full-path

              (= :directory kind)
              (gather-files (os/dir full-path) (string full-path sep) exclusions)))
          paths))


(defn- validate-project-data
  ```
  Check that the data in the project.janet file includes everything we need
  ```
  [data]
  (unless (and (data :project)
               (or (data :source)
                   (data :native)))
    (error "Project file must contain declare-project and declare-source"))
  data)


(defn parse-project
  ```
  Parse a project.janet file

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


(defn- generate-doc
  ```
  Generate an API document for a project
  ```
  [opts]
  (def project-file (opts :project-file))
  (def project-root (parent-dir project-file))
  (def project-data (parse-project project-file))
  (put opts :project-root project-root)

  (defn check-build-dir [name]
    (def path (string project-root "build" sep name ".so"))
    (when (= :file (os/stat path :mode))
      path))
  (array/push module/paths [check-build-dir :native])

  (def envs @{})
  (when-let [sources (get-in project-data [:source :source])]
    (def exclusions (map (fn [x] (string project-root x)) (opts :exclude)))
    (def paths (gather-files sources project-root exclusions))
    (reduce (fn [e p] (merge e (extract-env p))) envs paths))

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


(defn main
  [& argv]
  (def args (argy/parse-args config))
  (unless (or (args :help?) (args :error?))
    (def opts @{:defix (get (args :opts) "defix" "")
                :echo? (get (args :opts) "echo" false)
                :exclude (get (args :opts) "exclude" [])
                :include-private? (get (args :opts) "private" false)
                :link-parent ""
                :output-file (get (args :opts) "output" "api.md")
                :project-file (get (args :opts) "input" "project.janet")
                :template-file (get (args :opts) "template")})
    (try
      (generate-doc opts)
      ([err]
       (eprint "documentarian: " err)
       (os/exit 1)))))
