(import argy-bargy :as argy)
(import musty)


(def- sep (if (= :windows (os/which)) "\\" "/"))
(def- headings @{})


(def- config
  {:rules ["--defix" {:kind :single
                      :short "d"
                      :help "Remove a directory name from function names."}
           "--echo"  {:kind :flag
                      :short "e"
                      :help "Prints output to stdout."}
           "--input" {:kind :single
                      :short "i"
                      :help "Specify the project file."}
           "--output" {:kind :single
                       :short "o"
                       :help "Specify the output file."}
           "--private" {:kind :flag
                        :short "p"
                        :help "Include private values."}
           "--template" {:kind :single
                         :short "t"
                         :help "Specify a template file."}]
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


(defn- file-ext [path]
  (def last-dot (last (string/find-all "." path)))
  (string/slice path last-dot))


(defn- link
  ```
  Create a link to a specific line in a file
  ```
  [{:file file :line line} local-parent remote-parent]
  (if (nil? file)
    nil
    (if (and local-parent
             remote-parent
             (not (or (= "." local-parent)
                      (empty? local-parent))))
      (-> (string/replace local-parent remote-parent file)
          (string "#L" line))
      (string file "#L" line))))


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
   :link      (link item (opts :local-parent) (opts :remote-parent))
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
  (musty/render template {:project-name (get project :name)
                          :project-doc  (get project :doc)
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
  (let [ns              (or (meta :ns) maybe-ns)
        value           (or (meta :value) (first (meta :ref)))
        [file line col] (source-map meta)
        kind            (cond
                          (meta :macro) :macro
                          (meta :kind) (meta :kind)
                          (type value))
        private?        (meta :private)
        docs            (meta :doc)
        [sig docstring] (if (and docs (string/find "\n\n" docs))
                          (string/split "\n\n" docs 0 2)
                          [nil docs])]
    {:name      name
     :ns        ns
     :value     value
     :kind      kind
     :private?  private?
     :sig       sig
     :docstring docstring
     :file      file
     :line      line}))


(defn- path->ns
  ```
  Convert a path to a 'namespace'
  ```
  [path defix]
  (def ext (file-ext path))
  (string/slice path (length defix) (if (nil? ext) nil (inc (length ext)))))


(defn- find-aliases
  ```
  Find possible aliases

  Bindings that are imported into a namespace and then exported have a `meta`
  length of 1. This can be used as a heuristic to build a table of possible
  aliases that can be used in the `extract-bindings` function. A more robust
  implementation would store the value of the aliased binding and use that later
  to check.
  ```
  [envs defix]
  (def aliases @{})
  (each [path env] (pairs envs)
    (def ns (path->ns path defix))
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
  [envs include-private? defix]
  (def aliases (find-aliases envs defix))
  (defn ns-or-alias [name ns]
    (if-let [alias (get aliases name)
             _     (string/has-prefix? alias ns)]
      alias
      ns))
  (def bindings @[])
  (each [path env] (pairs envs)
    (def ns (path->ns path defix))
    (each [name meta] (pairs env)
      (when (and (document-name? name)
                 (or (not (get meta :private))
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


(defn- require-c
  ```
  Extract metadata information about C functions from the source file
  ```
  [path]
  (def env @{})
  (def cfuns-grammar
    # This assumes the enumeration of C functions in a particular mannner, see
    # https://github.com/pyrmont/markable/blob/077ec6b7618d4e775e59705b28806c865ba67238/src/markable/converter.c#L73-L109
    ~{:main (some (+ :cfuns 1))
      :cfuns (* :declaration :assignment (some :cfun) :sentinel)
      :declaration (* "static" :s+ "const" :s+ "JanetReg" (some (if-not "=" 1)))
      :assignment (* "=" :s* "{" :s*)
      :cfun (* "{" :name :pointer :docstring "}," :s*)
      :name (* :s* `"` (line) '(some (if-not `"` 1)) `"` :s* ",")
      :pointer (* :s* (some (+ :w :d (set "_-"))) :s* ",")
      :docstring (* :s* (% (some (* `"` (some (+ :escape '(if-not `"` 1))) `"` :s*))))
      :escape (+ (* `\n` (constant "\n"))
                 (* `\t` (constant "\t"))
                 (* `\"` (constant "\"")))
      :sentinel (* "{" :s* "NULL" :s* "," :s* "NULL" :s* "," :s* "NULL" :s* "}")})
  (def entry-grammar
    # This assumes the registration of C functions in a particular manner, see
    # https://github.com/pyrmont/markable/blob/077ec6b7618d4e775e59705b28806c865ba67238/src/markable/converter.c#L116-L118
    ~{:main (some (+ :entry 1))
      :entry (* "janet_cfuns" :s* "(" :s* "env" :s* "," :s* :ns :s* ",")
      :ns (+ "NULL" (* `"` '(some (+ :w :d (set "_-/"))) `"`))})
  (def source-code (slurp path))
  (when-let [fns (peg/match cfuns-grammar source-code)
             ns  (first (peg/match entry-grammar source-code))]
    (assert (zero? (% (length fns) 3)) "error parsing C source code")
    (each [line-num name docstring] (partition 3 fns)
      (put env name {:kind "cfunction"
                     :ns ns
                     :doc docstring
                     :source-map [path line-num 0]}))
    env))


(defn extract-env
  ```
  Extract the environment for a file in the project
  ```
  [source paths]
  (defn source->path [source paths]
    (if (string/has-prefix? (paths :project) source)
      (string/replace (paths :project) "" source)
      source))
  (cond
    (= ".c" (file-ext source))
    (let [path (source->path source paths)
          env  (require-c path)]
      {path env})

    (= ".janet" (file-ext source))
    (let [env    (dofile (if (= "." (paths :project)) (string "." sep source) source))
          path   (source->path source paths)]
      (put env :current-file nil)
      (put env :source nil)
      {path env})))


(defn gather-files
  ```
  Replace mixture of files and directories with files
  ```
  [paths &opt parent]
  (default parent ".")
  (mapcat (fn [path]
            (let [full-path (string parent sep path)]
              (case (os/stat full-path :mode)
                :file      full-path
                :directory (gather-files (os/dir full-path) full-path))))
          paths))


(defn- validate-project-data
  ```
  Check that the data in the project.janet file includes everything we need
  ```
  [data]
  (unless (and (get data :project)
               (or (get data :source)
                   (get data :native)))
    (error "Project file must contain declare-project and declare-source"))
  data)


(defn parse-project
  ```
  Parse a project.janet file

  This function returns a table of the values in the project file. The keys are
  the sections but without the leading `declare-` and inserted as keywords.
  ```
  [project-file]
  (let [contents (slurp project-file)
        p        (parser/new)
        result    @{}]
    (parser/consume p contents)
    (while (parser/has-more p)
      (let [form (parser/produce p)
            head (first form)
            tail (tuple/slice form 1)]
        (when (string/has-prefix? "declare-" head)
          (let [key (-> (string/replace "declare-" "" head) keyword)]
            (put result key (struct ;tail))))))
    (validate-project-data result)))


(defn- detect-dir
  ```
  Determine the project directory relative to the path to the project file
  ```
  [project-file]
  (let [seps (string/find-all sep project-file)]
    (if (empty? seps)
      "."
      (string/slice project-file 0 (+ 1 (array/peek seps))))))


(defn- generate-doc
  ```
  Generate an API document for a project
  ```
  [opts]
  (def project-file (opts :project-file))
  (def project-path (detect-dir project-file))
  (def project-data (parse-project project-file))
  (put opts :local-parent project-path)

  (def sources (-> (or (get-in project-data [:source :source])
                       (get-in project-data [:native :source]))
                   (gather-files project-path)))
  (put opts :include-ns? (not= 1 (length sources)))

  (def envs (reduce (fn [envs source]
                      (merge envs (extract-env source {:project project-path})))
                    @{}
                    sources))
  (def bindings (extract-bindings envs (opts :include-private?) (opts :defix)))
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
  (unless (args :help?)
    (def opts @{:defix (get (args :opts) "defix" "")
                :echo? (get (args :opts) "echo" false)
                :include-private? (get (args :opts) "private" false)
                :output-file (get (args :opts) "output" "api.md")
                :project-file (get (args :opts) "input" "project.janet")
                :remote-parent ""
                :template-file (get (args :opts) "template")})
    (generate-doc opts)))
