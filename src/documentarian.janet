(import spork/misc :as spork)

(var- include-private? false)

(defn- item-details
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
                               (string "  ")
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
  [{:file file :line line} base]
  (string base "/blob/master/" file "#L" line))


(defn- validate-project-data
  [data]
  (unless (and (get data :project)
               (get data :source))
    (error "Project file must contain declare-project and declare-source"))
  data)


(defn- parse-project
  []
  (def contents (slurp "project.janet"))
  (def p (parser/new))
  (def result @{})

  (parser/consume p contents)
  (while (parser/has-more p)
    (let [form (parser/produce p)
          head (first form)
          tail (tuple/slice form 1)]
      (case head
        'declare-project (put result :project (struct ;tail))
        'declare-source  (put result :source (struct ;tail)))))
  (validate-project-data result))


(defn- extract-from-dir
  [dir extract-fn]
  (reduce (fn [bindings path]
            (->> (string dir "/" path) (extract-fn) (merge bindings)))
          @{}
          (os/dir dir)))


(defn- extract-bindings
  [source]
  (if (string/has-suffix? ".janet" source)
    (let [path (string/replace ".janet" "" source)
          bindings (require path)]
      (each k [:current-file :source] (put bindings k nil))
      {source bindings})
    (extract-from-dir source extract-bindings)))


(defn- document-bindings
  [set-of-bindings module-name url]
  (def items @[])
  (each [filename bindings] (pairs set-of-bindings)
    (def module (->> (string/replace "src/" "" filename)
                     (string/replace ".janet" "")))
    (each [name meta] (pairs bindings)
      (if (or (nil? (get meta :private))
              include-private?)
        (array/push items (item-details (string module "/" name) meta)))))

  (sort-by (fn [item] (get item :name)) items)

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
                             (string/replace-all "/" "" (item :name)) ")"))
                   items)
              (string/join ",\n"))
          "\n\n" (string/join elements "\n\n\n") "\n"))


(defn main
  [& args]
  (set include-private? (and (get args 1) (= "-p" (get args 1))))

  (def project-data (parse-project))

  (def name (-> (get project-data :project) (get :name)))
  (def url (-> (get project-data :project) (get :url)))

  (def sources (-> (get project-data :source) (get :source)))

  (def bindings (reduce
                  (fn [bindings source]
                    (merge bindings (extract-bindings source)))
                  @{}
                  sources))

  (def document (document-bindings bindings name url))

  (spit "api.md" document))
