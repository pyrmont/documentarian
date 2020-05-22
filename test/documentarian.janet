(import testament :prefix "" :exit true)
(import ../src/documentarian :as doc)


(deftest parse-project
  (is (thrown? (doc/parse-project "project.janet"))))


(deftest parse-project-with-file
  (def result (doc/parse-project "fixtures/project.janet"))
  (is (= "Example" (get-in result [:project :name])))
  (is (= "https://github.com/pyrmont/example" (get-in result [:project :url])))
  (is (= ["src/example.janet"] (get-in result [:source :source]))))


(deftest extract-bindings-from-defs
  (def source "fixtures/defn.janet")
  (def [bindings-key bindings-val] (-> (doc/extract-bindings source "") pairs first))
  (def [item-key item-val] (-> (pairs bindings-val) first))
  (is (= source bindings-key))
  (is (= 'example item-key))
  (is (= "(example)\n\nThis is an example function" (item-val :doc)))
  (is (= [source 1 1] (item-val :source-map))))


(deftest extract-bindings-from-declared-vars
  (def source "fixtures/varfn.janet")
  (def [bindings-key bindings-val] (-> (doc/extract-bindings source "") pairs first))
  (def fn-name 'example)
  (def [item-key item-val] (->> (pairs bindings-val) (find |(= fn-name (first $)))))
  (is (= source bindings-key))
  (is (= fn-name item-key))
  (is (= "(example)\n\nThis is an example function" (item-val :doc)))
  (is (= [source 1 1] (item-val :source-map))))


(deftest extract-bindings-from-declared-vars
  (def source "fixtures/varfn.janet")
  (def [bindings-key bindings-val] (-> (doc/extract-bindings source "") pairs first))
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
  (def bindings {"example.janet" {'example {:private true}}})
  (def result (tuple ;(doc/bindings->items bindings)))
  (is (= [] result)))


(deftest bindings-with-items
  (var bindings {"example.janet" {'example {:value "Example"
                                            :doc "An example"
                                            :source-map ["example.janet" 1 1]}}})
  (var result (tuple ;(doc/bindings->items bindings)))
  (var expected [{:line 1
                  :value "Example"
                  :kind :string
                  :docstring "An example"
                  :file "example.janet"
                  :ns "example/"
                  :name 'example}])
  (is (= expected result))
  (set bindings {"example.janet" {'example {:ref ["Example"]}}})
  (set result (tuple ;(doc/bindings->items bindings)))
  (set expected [{:line nil
                  :value "Example"
                  :kind :string
                  :docstring nil
                  :file nil
                  :ns "example/"
                  :name 'example}])
  (is (= expected result)))


(deftest markdown-output
  (def expected
    (string "# Example API\n\n"
            "[example/example](#exampleexample)\n, [example/example2](#exampleexample2)\n\n"
            "## example/example\n\n"
            "**function**  | [source][1]\n\n\n"
            "This is an example.\n\n"
            "[1]: example.janet#L1\n\n"
            "## example/example2\n\n"
            "**function**  | [source][2]\n\n\n"
            "This is an example.\n\n"
            "[2]: example.janet#L2\n\n"))
  (def items
    [{:name 'example :ns "example/" :kind :function :docstring "This is an example." :file "example.janet" :line 1}
     {:name 'example2 :ns "example/" :kind :function :docstring "This is an example." :file "example.janet" :line 2}])
  (is (= expected (doc/items->markdown items "Example" {}))))


(run-tests!)
