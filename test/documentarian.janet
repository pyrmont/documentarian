(import /deps/testament/src/testament :prefix "" :exit true)


(review ../lib/documentarian :as doc)


(deftest parse-project
  (is (thrown? (doc/parse-project "fixtures/broken_project.janet"))))


(deftest parse-project-with-file
  (def actual (doc/parse-project "fixtures/project.janet"))
  (def expect {:project {:name "Example"
                         :url "https://github.com/pyrmont/example"
                         :doc "This is an example of project-level\ndocumentation."}
               :source  {:source ["src/example.janet"]}})
  (is (== expect actual)))


(deftest extract-env-from-defs
  (def source "fixtures/defn.janet")
  (def env (doc/extract-env source))
  (def actual (get-in env [source 'example]))
  (def expect {:doc "(example)\n\nThis is an example function" :source-map [source 1 1]})
  (is (== (expect :doc) (actual :doc)))
  (is (== (expect :source-map) (actual :source-map)))
  (is (== :function (type (actual :value)))))


(deftest extract-env-from-declared-vars-1
  (def source "fixtures/varfn.janet")
  (def env (doc/extract-env source))
  (def actual (get-in env [source 'example]))
  (def expect {:doc "(example)\n\nThis is an example function" :source-map [source 1 1]})
  (is (== (expect :doc) (actual :doc)))
  (is (== (expect :source-map) (actual :source-map)))
  (is (== :function (type (get-in actual [:ref 0])))))


(deftest extract-env-from-declared-vars-2
  (def source "fixtures/varfn.janet")
  (def env (doc/extract-env source))
  (def actual (get-in env [source 'example2]))
  (def expect {:doc "(example2)\n\nThis is an example function"})
  (is (== (expect :doc) (actual :doc)))
  (is (== :function (type (get-in actual [:ref 0])))))


(deftest gather-files
  (def actual (doc/gather-files ["test"]))
  (def expect ["./test/documentarian.janet"])
  (is (== actual expect)))


(deftest gather-files-with-exclusions
  (def actual (doc/gather-files ["fixtures"] "./" ["./fixtures"]))
  (def expect [])
  (is (== actual expect)))


(deftest bindings-with-private-items
  (def env {"./example.janet" {'example {:private true}}})
  (def opts {:include-private? false :defix "" :project-root "./"})
  (def actual (doc/extract-bindings env opts))
  (def expect [])
  (is (== expect actual)))


(deftest bindings-with-items
  (def env1 {"./example.janet" {'example {:value "Example"
                                        :doc "An example"
                                        :source-map ["example.janet" 1 1]}}})
  (def opts1 {:defix ""  :include-private? false :project-root "./"})
  (def actual1 (doc/extract-bindings env1 opts1))
  (def expect1 [{:line 1
                 :value "Example"
                 :kind :string
                 :docstring "An example"
                 :file "example.janet"
                 :ns "example"
                 :name 'example}])
  (is (== expect1 actual1))
  (def env2 {"./example.janet" {'example {:private false :ref ["Example"]}}})
  (def opts2 {:defix "" :include-private? false :project-root "./"})
  (def actual2 (doc/extract-bindings env2 opts2))
  (def expect2 [{:line nil
                 :value "Example"
                 :kind :string
                 :private? false
                 :docstring nil
                 :file nil
                 :ns "example"
                 :name 'example}])
  (is (== expect2 actual2)))


(deftest markdown-output
  (def expect
    (string "# Example API\n\n"
            "## example\n\n"
            "[example](#example), "
            "[example*](#example-1), "
            "[example2](#example2)\n\n"
            "## example\n\n"
            "**function**  | [source][1]\n\n\n"
            "This is an example.\n\n"
            "[1]: example.janet#L1\n\n"
            "## example*\n\n"
            "**function**  | [source][2]\n\n\n"
            "This is an example.\n\n"
            "[2]: example.janet#L2\n\n"
            "## example2\n\n"
            "**function**  | [source][3]\n\n\n"
            "This is an example.\n\n"
            "[3]: example.janet#L3\n\n"))
  (def bindings
    [{:name 'example :ns "example" :kind :function :docstring "This is an example." :file "example.janet" :line 1}
     {:name 'example* :ns "example" :kind :function :docstring "This is an example." :file "example.janet" :line 2}
     {:name 'example2 :ns "example" :kind :function :docstring "This is an example." :file "example.janet" :line 3}])
  (def actual (doc/emit-markdown bindings {:name "Example"} {}))
  (is (== expect actual)))


(deftest quotes-in-codeblocks
  (def sample-docstring
    ````
    A sample docstring with "quotes".

    ```
    A fenced code block with "quotes".
    ```
    ````)
  (def bindings
    [{:name 'example :ns "example" :kind :function :docstring sample-docstring}
     {:name :doc :ns "example" :kind :string :doc sample-docstring}])

  (let [lines (->> (doc/emit-markdown bindings {:name "Example" :doc sample-docstring} {})
                   (string/split "\n"))
        [project-docstring mod-docstring fn-docstring] (filter (partial string/find "sample") lines)
        [project-code-block mod-code-block fn-code-block] (filter (partial string/find "fenced") lines)]

    (each md [project-docstring mod-docstring fn-docstring]
      (is (== "A sample docstring with \"quotes\"." md)))
    (each md [project-code-block mod-code-block fn-code-block]
      (is (== "A fenced code block with \"quotes\"." md)))))


(run-tests!)
