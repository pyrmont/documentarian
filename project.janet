(declare-project
  :name "Documentarian"
  :description "Generate documentation from Janet source files"
  :author "Michael Camilleri"
  :license "MIT"
  :url "https://github.com/pyrmont/documentarian"
  :repo "git+https://github.com/pyrmont/documentarian"
  :dependencies ["https://github.com/pyrmont/argy-bargy"
                 "https://github.com/pyrmont/musty"]
  :dev-dependencies ["https://github.com/pyrmont/testament"])

(declare-executable
  :name "documentarian"
  :entry "documentarian.janet"
  :install true)

(declare-source
  :source ["documentarian.janet"])

(task "dev-deps" []
  (if-let [deps ((dyn :project) :dependencies)]
    (each dep deps
      (bundle-install dep))
    (do
      (print "no dependencies found")
      (flush)))
  (if-let [deps ((dyn :project) :dev-dependencies)]
    (each dep deps
      (bundle-install dep))
    (do
      (print "no dev-dependencies found")
      (flush))))
