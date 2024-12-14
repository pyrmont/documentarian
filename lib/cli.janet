(import ../deps/argy-bargy/argy-bargy :as argy)
(import ./documentarian :as doc)


(def config
  ```
  The configuration for Argy-Bargy
  ```
  {:rules ["--defix"       {:kind  :single
                            :short "d"
                            :proxy "prefix"
                            :help  "Remove <prefix> from all namespaces."}
           "--link-prefix" {:kind  :single
                            :short "L"
                            :proxy "url"
                            :help  "Use <url> as prefix for source code links."}
           "-------------------------------------------"
           "--only"        {:kind  :multi
                            :short "O"
                            :proxy "path"
                            :help  "Only create entries for bindings in <path> in the API document."}
           "--exclude"     {:kind  :multi
                            :short "x"
                            :proxy "path"
                            :help  "Exclude bindings in <path> from the API document."}
           "--private"     {:kind  :flag
                            :short "P"
                            :help  "Include private values in the API document."}
           "-------------------------------------------"
           "--project"     {:kind  :single
                            :short "p"
                            :proxy "path"
                            :help  "Use <path> as project file. (Default: project.janet)"}
           "--local"       {:kind  :flag
                            :short "l"
                            :help  "Set Janet's modpath to ./jpm_tree."}
           "--tree"        {:kind  :single
                            :short "t"
                            :proxy "path"
                            :help  "Set Janet's modpath to <path>."}
           "-------------------------------------------"
           "--echo"        {:kind  :flag
                            :short "e"
                            :help  "Output to stdout rather than output file."}
           "--out"         {:kind  :single
                            :short "o"
                            :proxy "path"
                            :help  "Use <path> as filename for the API document. (Default: api.md)"}
           "--template"    {:kind  :single
                            :short "T"
                            :proxy "path"
                            :help  "Use <path> as template for the API document."}
           "-------------------------------------------"]
   :info {:about "A document generation tool for Janet projects."}})


(defn args->opts
  ```
  Converts Argy-Bargy processed args into options for use with generate-doc
  ```
  [args]
  (def modpath (if (get-in args [:opts "local"]) "jpm_tree" (get-in args [:opts "tree"])))
  @{:defix (get-in args [:opts "defix"] "")
    :echo? (get-in args [:opts "echo"] false)
    :exclude (get-in args [:opts "exclude"] [])
    :include-private? (get-in args [:opts "private"] false)
    :link-prefix (get-in args [:opts "link-prefix"] "")
    :only (get-in args [:opts "only"])
    :output-file (get-in args [:opts "out"] "api.md")
    :project-file (get-in args [:opts "project"] "project.janet")
    :modpath (when modpath (string modpath doc/sep "lib"))
    :template-file (get-in args [:opts "template"])})


(defn run
  []
  (def parsed (argy/parse-args "documentarian" config))
  (def err (parsed :err))
  (def help (parsed :help))

  (cond
    (not (empty? help))
    (do
      (prin help)
      (os/exit (if (get-in parsed [:opts "help"]) 0 1)))

    (not (empty? err))
    (do
      (eprin err)
      (os/exit 1))

    (doc/generate-doc (args->opts parsed))))


# for testing in development
(defn- main [& args] (run))
