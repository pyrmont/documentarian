(declare-project
  :name "Documentarian"
  :description "Generate documentation from Janet source files"
  :author "Michael Camilleri"
  :license "MIT"
  :url "https://github.com/pyrmont/documentarian"
  :repo "git+https://github.com/pyrmont/documentarian"
  :dependencies ["https://github.com/janet-lang/spork"])

(declare-executable
  :name "documentarian"
  :entry "src/documentarian.janet")
