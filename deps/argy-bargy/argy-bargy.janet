# Global values

(var max-width "Maximum number of columns to use for usage messages" 120)
(var pad-inset "Number of columns to pad argument descriptions from the left" 4)
(var pad-right "Number of columns to pad argument descriptions from the right" 0)
(var hr "String to use to insert line breaks between argument descriptions" "---")

(var- cols nil)
(var- command nil)

(def- err @"")
(def- help @"")


# Utility functions

(defn- conform-args
  ```
  Conforms arguments

  In particular, splits short-options out if provided together.
  ```
  [args]
  (def res @[])
  (def grammar ~{:main      (+ :long-opt :short-opt :rest)
                 :rest      '(some 1)
                 :long-opt  (* '(* "--" (any (if-not "=" 1))) (? (* "=" :rest)))
                 :short-opt (* '(* "-" 1) (any (% (* (constant "-") '1))))})
  (def num-args (length args))
  (var i 0)
  (while (< i num-args)
    (def arg (args i))
    (when (= "--" arg)
      (array/concat res (array/slice args i))
      (break))
    (array/concat res (peg/match grammar arg))
    (++ i))
  res)


(defn- conform-rules
  ```
  Conforms rules
  ```
  [rules]
  (var rest-capture? false)
  (def orules @[])
  (def prules @[])
  (var help? false)

  (var i 0)
  (while (< i (length rules))
    (def k (get rules i))
    (if (string/has-prefix? hr k)
      (array/push orules [hr nil])
      (do
        (unless (or (string? k) (keyword? k))
          (errorf "names of rules must be strings or keywords: %p" k))
        (def v (get rules (++ i)))
        (when (nil? v)
          (errorf "number of elements in rules must be even: %p" rules))
        (unless (or (struct? v) (table? v))
          (errorf "each rule must be struct or table: %p" v))
        (unless (or (keyword? k) ({:flag true :count true :single true :multi true} (v :kind)))
          (errorf "each option rule must be of kind :flag, :count, :single or :multi: %p" v))
        (when (and (keyword? k) rest-capture? (v :splat?))
          (errorf "multiple parameter rules cannot capture :splat? %p" v))
        (cond
          (string? k)
          (let [name (if (string/has-prefix? "--" k) (string/slice k 2) k)]
            (when (nil? (peg/find '(* :w (some (+ :w (set "-_"))) -1) name))
              (errorf "option name must be at least two alphanumeric characters: %p" name))
            (array/push orules [name (merge v {:name name})])
            (set help? (= "help" name)))

          (keyword? k)
          (do
            (array/push prules [k v])
            (set rest-capture? (v :splat?))))))
    (++ i))

  (unless help?
    (array/push orules ["help" {:name   "help"
                                :kind   :help
                                :noex?  true
                                :short  "h"
                                :help   "Show this help message."}]))
  [orules prules])


(defn- conform-subconfigs
  ```
  Conforms subconfigs
  ```
  [subcommands]
  (def subconfigs @[])
  (var i 0)
  (while (< i (length subcommands))
    (def k (get subcommands i))
    (if (string/has-prefix? hr k)
      (array/push subconfigs [hr nil])
      (do
        (unless (string? k)
          (errorf "names of subcommands must be strings: %p" k))
        (def v (get subcommands (++ i)))
        (when (nil? v)
          (errorf "number of elements in subcommands must be even: %p" subcommands))
        (unless (or (struct? v) (table? v))
          (errorf "each subcommand must be struct or table: %p" v))
        (array/push subconfigs [k v])))
    (++ i))
  subconfigs)


(defn- get-cols
  ```
  Gets the columns in the terminal
  ```
  []
  (if cols
    cols
    (do
      (def win? (= :windows (os/which)))
      (def cmd
        (if win?
          ["powershell" "-command" "&{(get-host).ui.rawui.WindowSize.Width;}"]
          ["stty" "size"]))
      (with [f (file/temp)]
        (def exit-code (os/execute cmd :p {:out f :err nil}))
        (if (zero? exit-code)
          (do
            (file/seek f :set 0)
            (def out (string/trim (file/read f :all)))
            (def tcols (if win?
                         (scan-number out)
                         (scan-number (-> (string/split " " out) (get 1)))))
            (min tcols max-width))
          max-width)))))


(defn- get-rule
  ```
  Gets a rule matching a name
  ```
  [name rules]
  (var res nil)
  (each [k v] rules
    (when (or (= k name) (= (get v :short) name))
      (set res v)
      (break)))
  res)


(defn- get-subconfig
  ```
  Gets a subconfig matching a name
  ```
  [subconfigs name]
  (var res nil)
  (each [k v] subconfigs
    (when (= k name)
      (set res v)
      (break)))
  res)


(defn- split-words
  ```
  Splits a string into words
  ```
  [str]
  (def res @[])
  (def buf @"")
  (var i 0)
  (while (def curr-c (get str i))
    (++ i)
    (if (not (or (= 32 curr-c) (= 10 curr-c)) )
      (buffer/push buf curr-c)
      (when-let [next-c (get str i)]
        (++ i)
        (array/push res (string buf))
        (buffer/clear buf)
        (cond
          (= 10 next-c)
          (array/push res (string/from-bytes curr-c next-c))

          (= 32 next-c)
          nil

          (buffer/push buf next-c)))))
  (unless (empty? buf)
    (array/push res (string buf)))
  res)


(defn- stitch
  ```
  Stitches together components into a string

  Only adds a separator when adjacent components are non-nil.
  ```
  [parts &opt sep]
  (default sep " ")
  (string/join (filter truthy? parts) sep))


(defn- indent-str
  ```
  Indents a string by a number of spaces at the start

  If a maximum width is provided, wraps and indents lines by the hanging
  padding.
  ```
  [str startw &opt startp hangp maxw]
  (default startp 0)
  (default hangp 0)
  (default maxw cols)
  (def res (buffer (string/repeat " " startp)))
  (var currw startw)
  (var first? true)
  (each word (split-words str)
    (cond
      first?
      (do
        (buffer/push res word)
        (+= currw (length word))
        (set first? false))

      (= "\n\n" word)
      (do
        (buffer/push res word (string/repeat " " hangp))
        (set currw hangp)
        (set first? true))

      (< (+ currw 1 (length word)) maxw)
      (do
        (buffer/push res " " word)
        (+= currw (+ 1 (length word))))

      (do
        (buffer/push res "\n" (string/repeat " " hangp) word)
        (set currw (+ hangp (length word))))))
  res)


# Usage messages

(defn- usage-error
  ```
  Prints the usage error message to `err`
  ```
  [& msg]
  (when (and (empty? err) (empty? help))
    (xprint err command ": " ;msg)
    (xprint err "Try '" command " --help' for more information.")))


(defn- usage-parameters
  ```
  Prints the usage descriptions for the parameters to `help`
  ```
  [info rules]
  (def usages @[])
  (var pad 0)

  (each [name rule] rules
    (def proxy (or (rule :proxy) name))
    (def usage-prefix (string " " proxy))
    (def usage-help
      (stitch [(rule :help)
               (when (rule :default)
                 (string "(Default: " (rule :default) ")"))]))
    (array/push usages [usage-prefix usage-help])
    (set pad (max (+ pad-inset (length usage-prefix)) pad)))

  (unless (empty? usages)
    (xprint help)
    (if (info :params-header)
      (xprint help (info :params-header))
      (xprint help "Parameters:"))
    (xprint help)
    (each [prefix msg] usages
      (def startp (- pad (length prefix)))
      (xprint help prefix (indent-str msg (length prefix) startp pad (- cols pad-right))))))


(defn- usage-options
  ```
  Prints the usage descriptions for the options to `help`
  ```
  [info rules]
  (def usages @[])
  (var pad 0)

  (each [name rule] rules
    (if (= hr name)
      (array/push usages [nil nil])
      (do
        (def usage-prefix
          (stitch [(if (rule :short)
                     (string " -" (rule :short) ",")
                     "    ")
                   (string "--" name)
                   (when (or (= :single (rule :kind)) (= :multi (rule :kind)))
                     (string "<" (or (rule :proxy) name) ">"))
                   ]))
        (def usage-help
          (stitch [(rule :help)
                   (when (rule :default)
                     (string "(Default: " (rule :default) ")"))]))
        (array/push usages [usage-prefix usage-help])
        (set pad (max (+ pad-inset (length usage-prefix)) pad)))))

  (unless (empty? usages)
    (xprint help)
    (if (info :opts-header)
      (xprint help (info :opts-header))
      (xprint help "Options:"))
    (xprint help)
    (each [prefix msg] usages
      (if (nil? msg)
        (xprint help)
        (do
          (def startp (- pad (length prefix)))
          (xprint help prefix (indent-str msg (length prefix) startp pad (- cols pad-right))))))))


(defn- usage-subcommands
  ```
  Prints the usage descriptions for the subcommands to `help`
  ```
  [info subconfigs]
  (def usages @[])
  (var pad 0)

  (each [name config] subconfigs
    (if (= hr name)
      (array/push usages [nil nil])
      (do
        (def usage-prefix (string " " name))
        (def usage-help (get config :help ""))
        (array/push usages [usage-prefix usage-help])
        (set pad (max (+ pad-inset (length usage-prefix)) pad)))))

  (unless (empty? usages)
    (xprint help)
    (if (info :subs-header)
      (xprint help (info :subs-header))
      (xprint help "Subcommands:"))
    (xprint help)
    (each [prefix msg] usages
      (if (nil? msg)
        (xprint help)
        (do
          (def startp (- pad (length prefix)))
          (xprint help prefix (indent-str msg (length prefix) startp pad (- cols pad-right))))))
    (xprint help)
    (xprint help "For more information on each subcommand, type '" command " help <subcommand>'.")))


(defn- usage-example
  ```
  Prints a usage example to `help`
  ```
  [orules prules subconfigs]
  (xprint help
    (indent-str
      (string "Usage: "
              command
              ;(map (fn [[name rule]]
                      (unless (or (nil? rule) (rule :noex?))
                        (string " [--" name
                                (when (or (= :single (rule :kind))
                                          (= :multi (rule :kind)))
                                  (string " <" (or (rule :proxy) (rule :name)) ">"))
                                "]")))
                    orules)
              ;(map (fn [[name rule]]
                      (def proxy (or (rule :proxy) name))
                      (string " "
                              (unless (rule :req?) "[")
                              "<"
                              proxy
                              (when (rule :splat?) "...")
                              ">"
                              (unless (rule :req?) "]"))
                      )
                    prules)
              (unless (empty? subconfigs)
                " <subcommand> [<args>]"))
      0
      0
      (+ 7 (length command) 1)
      (- cols pad-right))))


(defn- usage
  ```
  Prints the usage message to `help`
  ```
  [config]
  (def info (get config :info {}))
  (def [orules prules] (conform-rules (get config :rules [])))
  (def subconfigs (conform-subconfigs (get config :subs [])))

  (when (and (empty? err) (empty? help))
    (if (info :usages)
      (each example (info :usages)
        (xprint help example))
      (usage-example orules prules subconfigs))

    (when (info :about)
      (xprint help)
      (xprint help (indent-str (info :about) 0)))

    (unless (empty? prules)
      (usage-parameters info prules))

    (usage-options info orules)

    (unless (empty? subconfigs)
      (usage-subcommands info subconfigs))

    (when (info :rider)
      (xprint help)
      (xprint help (indent-str (info :rider) 0)))))


# Processing functions

(defn- convert
  ```
  Converts a textual value using the converter

  Internal functions are called if the converter is one of the following:

  * `:string` - Returns the value as-is.
  * `:integer` - Converts the value to an integer.
  ```
  [arg converter]
  (if (nil? converter)
    arg
    (cond
      (keyword? converter)
      (case converter
        :string
        arg

        :integer
        (let [[ok? res] (protect (scan-number arg))]
          (when (and ok? (int? res))
            res)))

      (function? converter)
      (converter arg))))


(defn- consume-option
  ```
  Consumes an option
  ```
  [result orules args i &opt short?]
  (def opts (result :opts))
  (def shorts (result :shorts))
  (def arg (in args i))
  (def name (string/slice arg (if short? 1 2)))
  (if-let [rule (get-rule name orules)
           long-name (rule :name)
           kind (rule :kind)]
    (case kind
      :flag
      (do
        (put opts long-name true)
        (inc i))

      :count
      (do
        (put opts long-name (-> (opts long-name) (or 0) inc))
        (inc i))

      (if (or (= kind :single)
              (= kind :multi))
        (if-let [input (get args (inc i))]
          (if-let [val (convert input (rule :value))]
            (do
              (case kind
                :single (put opts long-name val)
                :multi  (put opts long-name (array/push (or (opts long-name) @[]) val)))
              (+ 2 i))
            (usage-error "'" input "' is invalid value for " arg))
          (usage-error "no value after option of type " kind))))
    (usage-error "unrecognized option '" arg "'")))


(defn- consume-param
  ```
  Consumes a parameter
  ```
  [result prule args i rem]
  (def params (result :params))
  (def arg (args i))
  (if-let [[name rule] prule]
    (if (rule :splat?)
      (do
        (def vals @[])
        (var j 0)
        (each a (array/slice args i (- -1 rem))
          (if-let [val (convert a (rule :value))]
            (do
              (array/push vals val)
              (++ j))
            (do
              (usage-error "'" a "' is invalid value for " (or (rule :proxy) name))
              (break))))
        (when (empty? err)
          (put params name vals)
          (+ i j)))
      (if-let [val (convert arg (rule :value))]
        (do
          (put params name val)
          (inc i))
        (usage-error "'" arg "' is invalid value for " (or (rule :proxy) name))))
    (usage-error "too many parameters passed")))


(defn- check-options
  ```
  Checks options
  ```
  [result rules]
  (def opts (result :opts))
  (each [name rule] rules
    (when (and (not (nil? rule)) (rule :default) (nil? (opts name)))
      (put-in result [:opts name] (rule :default)))))


(defn- check-params
  ```
  Checks params
  ```
  [result params rules]
  (def num-params (length params))
  (var i 0)
  (def num-rules (length rules))
  (var j 0)
  (while (< i num-params)
    (def rule (get rules j))
    (set i (consume-param result rule params i (- num-rules j 1)))
    (++ j))
  (while (< j num-rules)
    (def [name rule] (rules j))
    (if (rule :req?)
      (do
        (usage-error (or (rule :proxy) name) " is required")
        (break))
      (put-in result [:params name] (rule :default)))
    (++ j)))


(defn- check-subcommand
  ```
  Checks subcommands
  ```
  [result config]
  (unless (or (nil? (config :subs)) (result :sub))
    (usage config)))


# Parsing functions

(defn- parse-args-impl
  ```
  Parses arguments recursively if necessary to allow nested subcommands
  ```
  [name config]
  (set command name)

  (def [orules prules] (conform-rules (get config :rules [])))
  (def subconfigs (conform-subconfigs (get config :subs [])))
  (def args (conform-args (dyn :args)))

  (def result @{:cmd command :opts @{} :params (when (empty? subconfigs) @{})})
  (def params @[])

  (def num-args (length args))
  (var i 1)
  (while (< i num-args)
    (def arg (get args i))
    (set i (cond
             (or (= "--help" arg) (= "-h" arg))
             (do
               (put-in result [:opts "help"] true)
               (if (= "-h" arg)
                 (put-in result [:opts :h?] true))
               (usage config))

             (= "--" arg)
             (do
               (array/concat params (array/slice args (inc i)))
               (break))

             (string/has-prefix? "--" arg)
             (consume-option result orules args i)

             (= "-" arg)
             (do
               (array/push params arg)
               (inc i))

             (string/has-prefix? "-" arg)
             (consume-option result orules args i true)

             (empty? subconfigs)
             (do
               (array/push params arg)
               (inc i))

             (do
               (def help? (= "help" arg))
               (def subcommand (if help? (get args (inc i)) arg))
               (def subconfig (get-subconfig subconfigs subcommand))
               (if subcommand
                 (if subconfig
                   (if (not help?)
                     (with-dyns [:args (array/slice args i)]
                       (def subresult (if (nil? (subconfig :rules))
                                        @{:cmd subcommand :args (array/slice (dyn :args) 1)}
                                        (parse-args-impl (string command " " subcommand) subconfig)))
                       (put subresult :cmd subcommand)
                       (put result :sub subresult)
                       (break))
                     (do
                       (put-in result [:opts "help"] true)
                       (put result :sub @{:cmd subcommand})
                       (set command (string command " " subcommand))
                       (usage subconfig)))
                   (usage-error "unrecognized subcommand '" subcommand "'"))
                 (usage-error "no subcommand specified after 'help'")))))
    (when (nil? i)
      (break)))

  (when (and (empty? err) (empty? help))
    (check-subcommand result config)
    (check-options result orules)
    (check-params result params prules))

  result)

(defn parse-args
  ```
  Parses the `(dyn :args)` value for a program

  This function takes a `name` and a `config`. `name` is a string that
  represents the name of the program and is used for usage messages. `config`
  is a struct containing the following keys:

  * `:rules` - Tuple of rules to use to parse the arguments.
  * `:info` - Struct of messages to use in help output.

  The `config` struct may also contain a tuple of subcommands under the `:subs`
  key.

  ### Rules

  The rule tuple is a series of alternating rule names and rule contents. The
  rule name is either a string or a key. The rule contents is a struct.

  #### Options

  If the rule name is a string, the rule contents will be applied to option
  arguments (arguments that begin with a `-` or `--`). The rule contents struct
  can have the following keys:

  * `:kind` - The kind of option. Values are `:flag`, `:count`, `:single` and
    `:multi`. A flag is a binary choice (e.g. true/false, on/off) that can
    only occur once. A count is a monotonically increasing integral value that
    can occur one or more times. A single is a value that can only occur once.
    A multi is a value that can occur one or more times.
  * `:short` - A single letter that is used with `-` rather than `--` and can
    be combined with other short options (e.g. `-lah`).
  * `:help` - The help text for the option, displayed in the usage message.
  * `:default` - A default value that is used if the option occurs.
  * `:noex?` - Whether to hide the option from the generated usage example.
  * `:value` - A one-argument function that converts the text that is parsed to
    another kind of value. This function can be used for validation. If the
    return value is `nil`, the input is considered to fail parsing and a usage
    error message is printed instead.  Instead of a function, a keyword can be
    provided and Argy-Bargy's internal converter will be used instead. The
    valid keywords are :string and :integer.

  A `--help` option is added automatically unless provided in the rules tuple.
  Options will be separated by a blank line if the rules tuple includes a
  `---` separator.

  #### Parameters

  If the rule name is a keyword, the rule contents will be applied to parameter
  arguments (arguments that are not options). The rule contents struct can have
  the following keys:

  * `:help` - Help text for the parameter, displayed in the usage message.
  * `:default` - Default value that is used if the parameter is not present.
  * `:req?` - Whether the parameter is required to be present.
  * `:value` - One-argument function that converts the textual value that is
    parsed to a value that will be returned in the return struct. This function
    can be used for validation. If the return value is `nil`, the input is
    considered to fail parsing and a usage error message is printed instead.
    Instead of a function, a keyword can be provided and Argy-Bargy's converter
    will be used instead. The valid keywords are `:string` and `:integer`.
  * `:splat?` - Whether this rule should capture all unassigned parameters. Only
    one parameter rule can have `:splat?` set to `true`.

  ### Info

  The info struct contains messages that are used in the usage help. The struct
  can have the following keys:

  * `:about` - Message describing the program at a high level.
  * `:usages` - Collection of usage examples to be used in the usage message.
    If no examples are provided, one will be generated automatically based on
    the provided rules.
  * `:opts` - Message printed immediately prior to listing of options.
  * `:params` - Message printed immediately prior to listing of parameters.
  * `:rider` - Message printed at the end of the usage message.

  ### Subcommands

  The subcommands tuple is a series of alternating subcommand names and
  subcommand configs. The subcommand name is a string that will match the name
  of a subcommand. The config is a struct of the same form as the `config`
  struct. Multiple levels of subcommands can be supported by simply having a
  subcommand's `config` struct contain a `:subs` key with a subcommands tuple
  of its own.

  If the subcommand config struct does not contain a `:rules` key, parsing will
  stop and all subsequent arguments will be returned under an `:args` key. This
  can be useful for situations where the user wants to handle parsing in a
  separate function.

  In addition to names and configs, the tuple can contain instances of the
  string "---". When printing usage information, subcommands that were
  separated by a "---" will be separated by a line break.

  ### Return Value

  Once parsed, the return value is a table with `:cmd`, `:opts`, and either
  `:params` or `:sub` keys. The values associated with `:opts` and `:params`
  are tables containing the values parsed according to the rules. The table
  also includes `:err` and `:help` keys that contain either the error or help
  messages that may have been generated during parsing.
  ```
  [name config]
  (set cols (get-cols))
  (def res (-> (parse-args-impl name config)
               (merge-into {:err (string err) :help (string help)})))
  (buffer/clear err)
  (buffer/clear help)
  res)
