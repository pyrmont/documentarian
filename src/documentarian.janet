### Documentarian

## A document generation tool for Janet projects

## by Michael Camilleri
## 10 May 2020

## Thanks to Andrew Chambers for his feedback and suggestions.

(import spork/misc :as spork)


(def- *magic-indent*
  ```
  Janet indents all but the first line in docstrings by two spaces. For
  dedenting to work properly, we need to indent this initial line by
  the same number of spaces.
  ```
  "  ")


(var- include-private? false)


(defn- item-details
  ```
  Create a table of metadata
  ```
  [name meta]
  (let [[file line col] (get meta :source-map)
        value           (get meta :value)
        kind            (case (type value)
                          :function (if (get meta :macro) "macro" "function")
                          (type value))
        private?        (get meta :private)
        docs            (get meta :doc)
        [sig docstring] (if (string/find "\n\n" docs)
                          (->> docs
                               (string *magic-indent*)
                               (spork/dedent)
                               (|(string/split "\n\n" $ 0 2)))
                          [nil docs])]
   {:name      name
    :value     value
    :kind      kind
    :private?  private?
    :sig       sig
    :docstring docstring
    :file      file
    :line      line}))


(defn- link
  ```
  Create a link to a specific line in a file
  ```
  [{:file file :line line} base]
  (string base file "#L" line))


(defn- validate-project-data
  ```
  Check that the data in the project.janet file includes everything we need
  ```
  [data]
  (unless (and (get data :project)
               (get data :source))
    (error "Project file must contain declare-project and declare-source"))
  data)


(defn- parse-project
  ```
  Parse a project.janet file

  This function returns a table of the values in the project file. The keys are
  the sections but without the leading `declare-` and inserted as keywords.
  ```
  []
  (let [contents (slurp "project.janet")
        p        (parser/new)
        result    @{}]
    (parser/consume p contents)
    (while (parser/has-more p)
      (let [form (parser/produce p)
            head (first form)
            tail (tuple/slice form 1)]
        (when (string/has-prefix? "declare-" head)
          (let [key  (-> (string/replace "declare-" "" head) keyword)]
            (put result key (struct ;tail))))))
    (validate-project-data result)))


(defn- extract-from-dir
  ```
  Extract the bindings for the Janet files in a directory
  ```
  [dir extract-fn]
  (reduce (fn [bindings path]
            (->> (string dir "/" path) (extract-fn) (merge bindings)))
          @{}
          (os/dir dir)))


(defn- extract-bindings
  ```
  Extract the bindings for sources
  ```
  [source]
  (if (string/has-suffix? ".janet" source)
    (let [path (string/replace ".janet" "" source)
          bindings (require path)]
      (each k [:current-file :source] (put bindings k nil))
      {source bindings})
    (extract-from-dir source extract-bindings)))


(defn- bindings->items
  ```
  Convert the data structure used by Janet to a simple table
  ```
  [set-of-bindings]
  (def items @[])
  (each [filename bindings] (pairs set-of-bindings)
    (def module (->> (string/replace "src/" "" filename)
                     (string/replace ".janet" "")))
    (each [name meta] (pairs bindings)
      (if (or (nil? (get meta :private))
              include-private?)
        (array/push items (item-details (string module "/" name) meta)))))

  (sort-by (fn [item] (get item :name)) items))

(defn- items->markdown
  ```
  Create the Markdown-formatted strings
  ```
  [items module-name url]
  (var index 0)
  (def elements (seq [item :in items :before (++ index)]
                  (string "## "  (item :name) "\n\n"
                          "**" (item :kind) "** | "
                          (if(not (nil? (item :private?)))
                            "**private** | "
                            "")
                          "[source][s" index "]\n\n"
                          "```\n"
                          (if (= :function (type (item :value)))
                            (item :sig)
                            (string/format "%q" (item :value)))
                          "\n```\n\n"
                          (item :docstring)
                          "\n\n[s" index "]: " (link item url))))

  (string "# " module-name " API\n\n"
          (-> (map (fn [item]
                     (string "[`" (item :name) "`](#"
                             (->> (item :name)
                                  (string/replace-all "/" "")
                                  (string/replace-all "!" "")
                                  (string/replace-all "?" ""))
                             ")"))
                   items)
              (string/join ",\n"))
          "\n\n" (string/join elements "\n\n\n") "\n"))


(defn main
  [& args]
  (set include-private? (and (get args 1) (= "-p" (get args 1))))

  (def project-data (parse-project))

  (def name (-> (get project-data :project) (get :name)))
  (def url "")

  (def sources (-> (get project-data :source) (get :source)))

  (def bindings (reduce (fn [b s] (merge b (extract-bindings s))) @{} sources))
  (def items (bindings->items bindings))
  (def document (items->markdown items name url))

  (spit "api.md" document))
