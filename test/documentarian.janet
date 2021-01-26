(import spork/path)
(import testament :prefix "" :exit true)
(import ../src/documentarian :as doc)


(deftest parse-project
  (is (thrown? (doc/parse-project "project.janet"))))


(deftest parse-project-with-file
  (def result (doc/parse-project "fixtures/project.janet"))
  (is (= "Example" (get-in result [:project :name])))
  (is (= "https://github.com/pyrmont/example" (get-in result [:project :url])))
  (is (= "This is an example of project-level\ndocumentation." (get-in result [:project :doc])))
  (is (= ["src/example.janet"] (get-in result [:source :source]))))


(deftest extract-env-from-defs
  (def source "fixtures/defn.janet")
  (def [bindings-key bindings-val] (-> (path/join ".." source)
                                       (doc/extract-env {:project "../" :source ""})
                                       (pairs)
                                       (first)))
  (def [item-key item-val] (-> (pairs bindings-val) first))
  (is (= source bindings-key))
  (is (= 'example item-key))
  (is (= "(example)\n\nThis is an example function" (item-val :doc)))
  (is (= [source 1 1] (item-val :source-map))))


(deftest extract-env-from-declared-vars-v1
  (def source "fixtures/varfn.janet")
  (def [bindings-key bindings-val] (-> (path/join ".." source)
                                       (doc/extract-env {:project "../" :source ""})
                                       (pairs)
                                       (first)))
  (def fn-name 'example)
  (def [item-key item-val] (->> (pairs bindings-val) (find |(= fn-name (first $)))))
  (is (= source bindings-key))
  (is (= fn-name item-key))
  (is (= "(example)\n\nThis is an example function" (item-val :doc)))
  (is (= [source 1 1] (item-val :source-map))))


(deftest extract-env-from-declared-vars-v2
  (def source "fixtures/varfn.janet")
  (def [bindings-key bindings-val] (-> (path/join ".." source)
                                       (doc/extract-env {:project "../" :source ""})
                                       (pairs)
                                       (first)))
  (def fn-name 'example2)
  (def [item-key item-val] (->> (pairs bindings-val) (find |(= fn-name (first $)))))
  (is (= source bindings-key))
  (is (= fn-name item-key))
  (is (= "(example2)\n\nThis is an example function" (item-val :doc)))
  (is (= nil (item-val :source-map))))


(deftest gather-files
  (def result (tuple ;(doc/gather-files ["test"])))
  (is (= ["test/documentarian.janet"] result)))


(deftest bindings-with-private-items
  (def env {"example.janet" {'example {:private true}}})
  (def result (tuple ;(doc/extract-bindings env "")))
  (is (= [] result)))


(deftest bindings-with-items
  (var env {"example.janet" {'example {:value "Example"
                                       :doc "An example"
                                       :source-map ["example.janet" 1 1]}}})
  (var result (tuple ;(doc/extract-bindings env "")))
  (var expected [{:line 1
                  :value "Example"
                  :kind :string
                  :docstring "An example"
                  :file "example.janet"
                  :ns "example"
                  :name 'example}])
  (is (= expected result))
  (set env {"example.janet" {'example {:private false :ref ["Example"]}}})
  (set result (tuple ;(doc/extract-bindings env "")))
  (set expected [{:line nil
                  :value "Example"
                  :kind :string
                  :private? false
                  :docstring nil
                  :file nil
                  :ns "example"
                  :name 'example}])
  (is (= expected result)))


(deftest markdown-output
  (def expected
    (string "# Example API\n\n"
            "[example/example](#exampleexample)\n, "
            "[example/example*](#exampleexample-1)\n, "
            "[example/example2](#exampleexample2)\n\n"
            "## example/example\n\n"
            "**function**  | [source][1]\n\n\n"
            "This is an example.\n\n"
            "[1]: example.janet#L1\n\n"
            "## example/example*\n\n"
            "**function**  | [source][2]\n\n\n"
            "This is an example.\n\n"
            "[2]: example.janet#L2\n\n"
            "## example/example2\n\n"
            "**function**  | [source][3]\n\n\n"
            "This is an example.\n\n"
            "[3]: example.janet#L3\n\n"))
  (def bindings
    [{:name 'example :ns "example" :kind :function :docstring "This is an example." :file "example.janet" :line 1}
     {:name 'example* :ns "example" :kind :function :docstring "This is an example." :file "example.janet" :line 2}
     {:name 'example2 :ns "example" :kind :function :docstring "This is an example." :file "example.janet" :line 3}])
  (is (= expected (doc/emit-markdown bindings {:name "Example"} {}))))


(run-tests!)
