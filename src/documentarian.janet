### Documentarian

## A document generation tool for Janet projects

## by Michael Camilleri
## 10 May 2020

## Thanks to Andrew Chambers for his feedback and suggestions.


(import spork/misc :as spork)
(import musty :as musty)


(var- include-private? false)


(def- template
  (spork/dedent
    ````
    # {{module}} API

    {{#elements}}
    {{^first}}, {{/first}}[{{name}}](#{{in-link}})
    {{/elements}}

    {{#elements}}
    ## {{name}}

    **{{kind}}** {{#private?}}| **private**{{/private?}} {{#link}}| [source][{{num}}]{{/link}}

    {{#sig}}
    ```janet
    {{sig}}
    ```
    {{/sig}}
    {{docstring}}

    {{#link}}
    [{{num}}]: {{link}}
    {{/link}}

    {{/elements}}
    ````))


(defn- link
  ```
  Create a link to a specific line in a file
  ```
  [{:file file :line line} base]
  (string base file "#L" line))


(defn item->element
  ```
  Prepare the fields for the template
  ```
  [item num]
  {:num       num
   :first     (one? num)
   :name      (item :name)
   :kind      (string (item :kind))
   :private?  (item :private?)
   :sig       (or (item :sig)
                  (and (not (nil? (item :value)))
                       (string/format "%q" (item :value))))
   :docstring (item :docstring)
   :link      (link item "")
   :in-link   (->> (item :name)
                   (string/replace-all "/" "")
                   (string/replace-all "!" "")
                   (string/replace-all "?" ""))})


(defn items->markdown
  ```
  Create the Markdown-formatted strings
  ```
  [items module-name url]
  (let [elements (seq [i :range [0 (length items)]]
                   (item->element (items i) (+ 1 i)))]
    (musty/render template {:module module-name :elements elements})))


(defn- item-details
  ```
  Create a table of metadata
  ```
  [name meta]
  (let [value           (or (meta :value) (first (meta :ref)))
        [file line col] (or (meta :source-map) [nil nil nil])
        kind            (if (meta :macro) :macro (type value))
        private?        (meta :private)
        docs            (meta :doc)
        [sig docstring] (if (and docs (string/find "\n\n" docs))
                          (->> (string/split "\n\n" docs 0 2)
                               (map spork/dedent))
                          [nil docs])]
    {:name      name
     :value     value
     :kind      kind
     :private?  private?
     :sig       sig
     :docstring docstring
     :file      file
     :line      line}))


(defn bindings->items
  ```
  Convert the data structure used by Janet to a simple table
  ```
  [set-of-bindings]
  (def items @[])
  (each [filename bindings] (pairs set-of-bindings)
    (def module (->> filename (string/replace "src/" "") (string/replace ".janet" "")))
    (each [name meta] (pairs bindings)
      (if (or (not (get meta :private))
              include-private?)
        (array/push items (item-details (string module "/" name) meta)))))
  (sort-by (fn [item] (get item :name)) items))


(defn extract-bindings
  ```
  Extract the bindings for sources
  ```
  [source]
  (when (string/has-suffix? ".janet" source)
    (let [path (string/replace ".janet" "" source)
          bindings (require path)]
      (each k [:current-file :source] (put bindings k nil))
      {source bindings})))


(defn gather-files
  ```
  Replace mixture of files and directries with files
  ```
  [paths &opt parent]
  (default parent "")
  (def sep (if (= :windows (os/which)) "\\" "/"))
  (mapcat
    (fn [path]
      (let [p (string parent
                      (if (empty? parent) "" sep)
                      path)]
        (case (os/stat p :mode)
          :file
          p

          :directory
          (gather-files (os/dir p) p))))
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
  [&opt project-file]
  (default project-file "project.janet")
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


(defn main
  [& args]
  (set include-private? (and (get args 1) (= "-p" (get args 1))))

  (def project-data (parse-project))

  (def name (get-in project-data [:project :name]))
  (def sources (-> (get-in project-data [:source :source]) gather-files))

  (def bindings (reduce (fn [b s] (merge b (extract-bindings s))) @{} sources))
  (def items (bindings->items bindings))
  (def document (items->markdown items name ""))

  (spit "api.md" document))
