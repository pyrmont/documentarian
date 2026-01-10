# character values

(def- tb  9)
(def- nl 10)
(def- cr 13)
(def- sp 32)
(def- em 33)
(def- dq 34)
(def- am 38)
(def- lt 60)
(def- es 61)
(def- gt 62)

# newline-excluding whitespace

(def- hs (string/from-bytes tb nl cr sp))

# current working directory

# state

(def- default-node
  @{:id nil
    :tag nil
    :parent nil
    :ctx nil
    :value nil})

(var- ctd nil)
(var- ctf nil)
(var- dcl nil)
(var- dop nil)
(var- line nil)
(var- node nil)
(var- no-tags? nil)
(var- only-ws? nil)
(var- out nil)
(var- pos nil)
(var- root nil)
(var- t nil)
(var- tag nil)

(defn- copy [v]
  (case (type v)
    # table
    :table
    (do
      (def c (table))
      (eachk k v
        (put c k (copy (get v k))))
      c)
    # array
    :array
    (do
      (def c (array))
      (each e v
        (array/push c (copy e)))
      c)
    # buffer
    :buffer
    (buffer v)
    # default
    v))

(defn- load [state]
  (set ctd (get state :ctd))
  (set ctf (get state :ctf))
  (set dcl (get state :dcl))
  (set dop (get state :dop))
  (set line (get state :line))
  (set node (get state :node))
  (set no-tags? (get state :no-tags?))
  (set only-ws? (get state :only-ws?))
  (set out (get state :out))
  (set pos (get state :pos))
  (set root (get state :root))
  (set t (get state :t))
  (set tag (get state :tag)))

(defn- reset []
  (set ctd nil)
  (set ctf nil)
  (set dcl "}}")
  (set dop "{{")
  (set line @"")
  (set node nil)
  (set no-tags? true)
  (set only-ws? true)
  (set out @"")
  (set pos 0)
  (set root nil)
  (set t nil)
  (set tag nil))

(defn- save []
  (def state @{})
  (put state :ctd ctd)
  (put state :ctf ctf)
  (put state :dcl dcl)
  (put state :dop dop)
  (put state :line line)
  (put state :node node)
  (put state :no-tags? no-tags?)
  (put state :only-ws? only-ws?)
  (put state :out out)
  (put state :pos pos)
  (put state :root root)
  (put state :t t)
  (put state :tag tag)
  state)

# parser

(var- parse* (fn :parse* [template &opt cwd cwf]))

# independent utility functions

(defn- add-child [parent child]
  (def children (get parent :value))
  (assert (array? children) "(syntax) parent not containing node")
  (array/push children child))

(defn- add-line []
  (unless (and only-ws?
               (not no-tags?))
    (buffer/push out line))
  (buffer/clear line)
  (set only-ws? true)
  (set no-tags? true))

(defn- dir [path]
  (if (or (string/has-suffix? "/" path) (string/has-suffix? "\\" path))
    (break path))
  (def s (get {:mingw "\\" :windows "\\"} (os/which) "/"))
  (string path s))

(defn- escape [s]
  (def res @"")
  (var i 0)
  (while (def c (get s i))
    (case c
      # double quote
      dq
      (buffer/push res "&quot;")
      # ampersand
      am
      (buffer/push res "&amp;")
      # less than
      lt
      (buffer/push res "&lt;")
      # greater than
      gt
      (buffer/push res "&gt;")
      # default
      (buffer/push res c))
    (++ i))
  (string res))

(defn- leading-ws []
  (if (string/check-set (string/from-bytes tb sp) line)
    (string line)
    ""))

(defn- not-eof? []
  (< pos (length t)))

(defn- to-string [v]
  (cond
    (nil? v)
    ""
    (or (string? v) (buffer? v))
    v
    # default
    (describe v)))

# dependent utility functions

(defn- delim? [delim]
  (var i 0)
  (while (and (not-eof?) (< i (length delim)))
    (def c (get t pos))
    (def d (get delim i))
    (unless (= c d)
      (break))
    (++ pos)
    (++ i))
  (= i (length delim)))

(defn- end-text
  []
  (when (empty? line)
    (break))
  (def new-node (copy default-node))
  (put new-node :tag :text)
  (put new-node :parent node)
  (put new-node :value (string line))
  (buffer/clear line)
  (add-child node new-node))

# tag checking

(defn- tag? [dop dcl]
  (def prv pos)
  (unless (delim? dop)
    (set pos prv)
    (break))
  (var closed? false)
  # TODO Handle syntax errors
  (while (not-eof?)
    (when (delim? dcl)
      (set closed? true)
      (break))
    (++ pos))
  (unless closed?
    (set pos prv)
    (break))
  (set tag (string/slice t prv pos)))

# node generating

(defn- do-comment []
  (end-text)
  (def new-node (copy default-node))
  (put new-node :tag :comment)
  (put new-node :parent node)
  (add-child node new-node)
  (set tag nil))

(defn- do-delim []
  (end-text) # TODO check this doesn't cause empty lines
  (def begin (+ 0 (inc (length dop))))
  (def end (- -1 (inc (length dcl))))
  (def [op cl] (->> (string/slice tag begin end)
                    (string/trim)
                    (string/split " ")
                    (filter (comp not empty?))))
  (set dop op)
  (set dcl cl)
  (def new-node (copy default-node))
  (put new-node :tag :delimiter)
  (put new-node :parent node)
  (add-child node new-node)
  (set tag nil))

(defn- do-interpolate [&named raw? triple?]
  (end-text)
  (def begin (+ 0 (length dop) (if raw? 1 0)))
  (def end (- -1 (length dcl) (if triple? 1 0)))
  (def id (->> (string/slice tag begin end)
               (string/trim)))
  (def new-node (copy default-node))
  (put new-node :id id)
  (put new-node :tag :variable)
  (put new-node :raw? raw?)
  (put new-node :parent node)
  (add-child node new-node)
  (set tag nil))

(defn- do-partial []
  (assert (not (nil? ctd)) "cannot parse partials if dir not provided to render")
  (end-text)
  (def begin (+ 0 (length dop) 1))
  (def end (- -1 (length dcl)))
  (def id (->> (string/slice tag begin end)
               (string/trim)))
  (def new-node (copy default-node))
  (put new-node :id id)
  (put new-node :tag :partial)
  (put new-node :parent node)
  (put new-node :value (string (dir ctd) id ".mustache"))
  (add-child node new-node)
  (set tag nil))

(defn- do-section-begin [&named invert?]
  (end-text)
  (def begin (+ 0 (length dop) 1))
  (def end (- -1 (length dcl)))
  (def id (->> (string/slice tag begin end)
               (string/trim)))
  (def new-node (copy default-node))
  (put new-node :id id)
  (put new-node :tag :section)
  (put new-node :invert? invert?)
  (put new-node :parent node)
  (put new-node :value @[])
  (add-child node new-node)
  (set node new-node)
  (set tag nil))

(defn- do-section-end []
  (end-text)
  (def begin (+ 0 (length dop) 1))
  (def end (- -1 (length dcl)))
  (def id (->> (string/slice tag begin end)
               (string/trim)))
  (assert (= (get node :id) id) "(syntax) section name does not match at begin and end")
  (set node (get node :parent))
  (set tag nil))

(defn- do-char []
  (def c (get t pos))
  (if (= nl c)
    (do
      (end-text)
      (def new-node (copy default-node))
      (put new-node :tag :newline)
      (put new-node :parent node)
      (add-child node new-node))
    (buffer/push line c))
  (++ pos))

# parsing

(set parse* (fn :parse*
  [template cwd &opt cwf]
  (reset)
  (set ctd cwd)
  (set ctf cwf)
  (set root (copy default-node))
  (put root :tag :root)
  (put root :value @[])
  (set node root)
  (set t template)
  (while (not-eof?)
    (cond
      # comment
      (tag? (string dop "!") dcl)
      (do-comment)
      # custom delimiter
      (tag? (string dop "=") (string "=" dcl))
      (do-delim)
      # section begin
      (tag? (string dop "#") dcl)
      (do-section-begin)
      # inverted section begin
      (tag? (string dop "^") dcl)
      (do-section-begin :invert? true)
      # section end
      (tag? (string dop "/") dcl)
      (do-section-end)
      # partial
      (tag? (string dop ">") dcl)
      (do-partial)
      # triple mustache raw interpolation
      (and (= "{{" dop) (tag? (string dop "{") (string "}" dcl)))
      (do-interpolate :raw? true :triple? true)
      # raw interpolation
      (tag? (string dop "&") dcl)
      (do-interpolate :raw? true)
      # escaped interpolation
      (tag? dop dcl)
      (do-interpolate)
      # default
      (do-char)))
  (end-text)
  root))

# resolving

(defn- interpolate [node]
  (def id (get node :id))
  (if (= "." id)
    (break (get node :ctx)))
  (def ks (string/split "." id))
  (var res nil)
  (var n node)
  (var found? false)
  (while (not (nil? n))
    (var ctx (get n :ctx))
    (each k ks
      (set ctx (or (get ctx k) (get ctx (keyword k))))
      (if (nil? ctx)
        (break)
        (set found? true)))
    (set res ctx)
    (if found?
      (break))
    (set n (get n :parent)))
  res)

(defn- resolve [node]
  (case (get node :tag)
    :root
    (each n (get node :value [])
      (put n :ctx (get node :ctx))
      (resolve n))
    :comment
    (set no-tags? false)
    :delimiter
    (set no-tags? false)
    :newline
    (do
      (buffer/push line "\n")
      (add-line))
    :partial
    (do
      (def path (get node :value))
      (when (and (not= ctf path) (= :file (os/stat path :mode)))
        (def old-only-ws? only-ws?)
        (def ws (leading-ws))
        (def contents (slurp path))
        (def old-state (save))
        (def p-root (parse* contents ctd path))
        (load old-state)
        (var prev-nl? false)
        (each p-node (get p-root :value)
          (when prev-nl?
            (set prev-nl? false)
            (cond
              # text nodes
              (= :text (get p-node :tag))
              (put p-node :value (string ws (get p-node :value)))
              # non-newline nodes
              (not= :newline (get p-node :tag))
              (do
                (def text-node (copy default-node))
                (put text-node :tag :text)
                (put text-node :parent (get node :ctx))
                (put text-node :value ws)
                (resolve text-node))))
          (if (= :newline (get p-node :tag))
            (set prev-nl? true))
          (put p-node :parent (get node :ctx))
          (put p-node :ctx (get node :ctx))
          (resolve p-node)
          (add-line))
        (set only-ws? old-only-ws?))
      (set no-tags? false))
    :section
    (do
      (set no-tags? false)
      (def val (interpolate node))
      (def invert? (get node :invert?))
      (cond
        (and (or (not val)
                 (and (indexed? val) (empty? val)))
             invert?)
        (each n (get node :value)
          (put n :ctx (get node :ctx))
          (resolve n))
        (and val (not invert?))
        (do
          (def group (if (indexed? val) val [val]))
          (each item group
            (each n (get node :value)
              (put n :ctx item)
              (resolve n)))))
      (set no-tags? false))
    :text
    (do
      (if only-ws?
        (set only-ws? (string/check-set hs (get node :value))))
      (buffer/push line (get node :value)))
    :variable
    (do
      (def val (interpolate node))
      (when val
        (def s (to-string val))
        (->> (if (get node :raw?) s (escape s))
             (buffer/push line))
        (set only-ws? false))
      (set no-tags? false))
    # impossible
    (error "unrecognised node type")))

# public API

(defn render
  ```
  Renders a Mustache `template` using the provided `context` in `dir`

  The directory `dir` is used to load partial templates. If not provided,
  an error will be raised if a partial is encountered.
  ```
  [template context &named dir]
  (def root (parse* template dir))
  (put root :ctx context)
  (resolve root)
  (add-line)
  (string out))
