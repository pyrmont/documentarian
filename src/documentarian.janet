(import musty)
(import spork/argparse)
(import spork/path)


(def- sep path/sep)
(var- include-private? false)
(def- headings @{})


(defn- path/without-ext [path]
  (def txe (string/reverse (path/ext path)))
  (if (nil? txe)
    path
    (->> (string/reverse path)
       (string/replace txe "")
       (string/reverse))))


(def- arg-settings
  ["A document generation tool for Janet projects."
   "defix" {:kind :option
            :short "d"
            :help "Remove this prefix from binding names."
            :default "src"}
   "echo"  {:kind :flag
            :short "e"
            :help "Prints output to stdout."}
   "input" {:kind :option
            :short "i"
            :help "Specify the project file."
            :default "project.janet"}
   "output" {:kind :option
             :short "o"
             :help "Specify the output file."
             :default "api.md"}
   "private" {:kind :flag
              :short "p"
              :help "Include private values."}])


(def- template
  ````
  # {{project-name}} API

  {{#project-doc}}
  {{project-doc}}

  {{/project-doc}}
  {{#modules}}
  ## {{ns}}

  {{#items}}
  {{^first}}, {{/first}}[{{name}}](#{{in-link}})
  {{/items}}

  {{#doc}}
  {{doc}}

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
   :in-link   (in-link (string (item :ns) "/" (item :name)))})


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
  [name meta ns]
  (let [value           (or (meta :value) (first (meta :ref)))
        [file line col] (source-map meta)
        kind            (if (meta :macro) :macro (type value))
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
  (defn defix-path [p defix]
    (if (empty? defix)
      p
      (string/replace (string defix sep) "" p)))
  (-> (path/without-ext path)
      (defix-path defix)))


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


(defn extract-bindings
  ```
  Extract information about the bindings from the environments
  ```
  [envs defix]
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
      (when (or (not (get meta :private))
                include-private?)
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
  [source paths]
  (when (= ".janet" (path/ext source))
    (let [module (path/without-ext source)
          env    (require (if (= "." (paths :project)) (string "." sep module) module))
          path   (if (string/has-prefix? (paths :project) source)
                   (string/replace (paths :project) "" source)
                   source)]
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
            (let [full-path (path/join parent path)]
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
               (get data :source))
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
  # (-> (path/abspath project-file)
  #     (path/dirname)))
  (let [seps (string/find-all sep project-file)]
    (if (empty? seps)
      "."
      (string/slice project-file 0 (+ 1 (array/peek seps))))))


(defn- generate-doc
  ```
  Generate the document based on various inputs
  ```
  [&keys {:echo    echo?
          :output  output-file
          :private private
          :input   project-file
          :defix   defix}]
  (when private
    (set include-private? true))
  (def project-path (detect-dir project-file))
  (def project-data (parse-project project-file))
  (def sources (-> (get-in project-data [:source :source])
                   (gather-files project-path)))
  (def envs (reduce (fn [envs source]
                      (merge envs (extract-env source {:project project-path})))
                    @{}
                    sources))
  (def bindings (extract-bindings envs defix))
  (def document (emit-markdown bindings
                               {:name (get-in project-data [:project :name])
                                :doc  (get-in project-data [:project :doc])}
                               {:local-parent  project-path
                                :remote-parent ""}))
  (if echo?
    (print document)
    (spit output-file document)))


(defn main
  [& args]
  (let [result (argparse/argparse ;arg-settings)]
    (unless result
      (os/exit 1))
    (generate-doc :echo    (result "echo")
                  :output  (result "output")
                  :private (result "private")
                  :input   (result "input")
                  :defix   (result "defix"))))
