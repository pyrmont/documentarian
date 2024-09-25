(def- messages
  {:section-tag-mismatch
   "Syntax error: The opening and closing section tags do not match"

   :syntax-error
   "Syntax error at index %d: %q"})


(defn- syntax-error
  ```
  Raise a syntax error specifying the `col` and `fragment`
  ```
  [col fragment]
  (error (string/format (messages :syntax-error) col fragment)))


(defn- inverted
  ```
  Return the computed `data` if the tag name in `open-id` and `close-id` does
  not exist
  ```
  [open-id data ws close-id]
  (unless (= open-id close-id) (error (messages :section-tag-mismatch)))
  ~(let [val (lookup ,(keyword open-id))]
     (if (or (nil? val) (and (indexed? val) (empty? val)))
       (string ,data ,ws)
       "")))


(defn- section
  ```
  Return the computed `data` if the tag name in `open-id` and `close-id` exists
  or is a non-empty list

  If the tag represents:

  1. a non-empty list, will concatenate the generated value;
  2. a truthy value, will return the generated value.
  ```
  [open-id data ws close-id]
  (unless (= open-id close-id) (error (messages :section-tag-mismatch)))
  ~(let [val (lookup ,open-id)]
     (cond
       (indexed? val)
       (string ;(seq [el :in val
                         :before (array/push ctx el)
                         :after (array/pop ctx)]
                  (string ,data ,ws)))
       (dictionary? val)
       (defer (array/pop ctx)
         (array/push ctx val)
         (string ,data ,ws))

       val
       (string ,data ,ws)

       :else
       "")))

(defn- variable
  ```
  Return the HTML-escaped computed value `x`
  ```
  [x &keys {:escape? escape?}]
  (default escape? true)
  ~(let [val (-> ,x lookup (or "") string)]
     ,(if escape? '(escape val) 'val)))


(defn- variable-unescaped
  ```
  Return the unescaped computer value `x`
  ```
  [x]
  (variable x :escape? false))


(defn- text
  ```
  Return the text `x`
  ```
  [x]
  x)


(defn- data
  ```
  Concatenate the values `xs`
  ```
  [& xs]
  ~(string ,;xs))


(defn- debugger
  [& xs]
  (print (string/format "%j" (gensym)))
  (print (string/format "%j" xs)))


(def- mustache
  ```
  The grammar for Mustache
  ```
  (peg/compile
    ~{:end-or-error (+ -1 (cmt '(* ($) (between 1 10 1)) ,syntax-error))

      # :start (drop (cmt (* ($) (constant 0)) ,=))
      # :debug (drop (cmt (* (argument 0) (constant "Here: ") ($)) ,debugger))

      :newline (* (? "\r") "\n")
      :inspace (any (set " \t\v"))

      :identifier (* :s* (+ "." (* :w (any (if-not (set "{}") :S)))) :s*)
      :delim-close "}}"
      :delim-open "{{"

      :standalone-trail (+ (* (? "\r") "\n") (* :inspace -1))
      # Why does this only work with a double negative for the lookbehind?
      :standalone-lead (* (not (> -1 (not "\n"))) :inspace)

      :tag-close-inline (* ':inspace :delim-open "/" ':identifier :delim-close)
      :tag-close-standalone (* :standalone-lead :tag-close-inline :standalone-trail)
      :tag-close (+ :tag-close-standalone :tag-close-inline)

      :partial (* "{{> " :identifier "}}")

      :comments-inline (* :delim-open "!" (any (if-not :delim-close 1)) :delim-close)
      :comments-standalone (* :standalone-lead :comments-inline :standalone-trail)
      :comments (+ :comments-standalone :comments-inline)

      :inverted-open-inline (* :delim-open "^" ':identifier :delim-close)
      :inverted-open-standalone (* :standalone-lead :inverted-open-inline :standalone-trail)
      :inverted-open (+ :inverted-open-standalone :inverted-open-inline)
      :inverted (/ (* :inverted-open :data :tag-close) ,inverted)

      :section-open-inline (* :delim-open "#" ':identifier :delim-close)
      :section-open-standalone (* :standalone-lead :section-open-inline :standalone-trail)
      :section-open (+ :section-open-standalone :section-open-inline)
      :section (/ (* :section-open :data :tag-close) ,section)

      :unescape-variable-ampersand (* :delim-open "&" (/ ':identifier ,variable-unescaped) :delim-close)
      :unescape-variable-triple (* :delim-open "{" (/ ':identifier ,variable-unescaped) "}" :delim-close)
      :variable (/ (* :delim-open ':identifier :delim-close) ,variable)

      :variables (+ :variable :unescape-variable-triple :unescape-variable-ampersand)
      :others (+ :section :inverted :comments :partial)
      :tag (+ :others :variables)

      :text (/ '(some (if-not (+ "\n" (* :inspace :delim-open)) 1)) ,text)
      :newlines (/ '(some :newline) ,text)
      :trailing (/ '(* :inspace (! (* :delim-open "/"))) ,text)

      :data (/ (any (+ :newlines :text :tag :trailing)) ,data)
      :main (* :data :end-or-error)}))


(defn- escape
  ```
  Escape the `str` of HTML entities
  ```
  [str]
  (def translations
    {34 "&quot;"
     38 "&amp;"
     39 "&apos;"
     60 "&lt;"
     62 "&gt;"})
  (def result @"")
  (each byte str
    (if-let [replacement (get translations byte)]
      (buffer/push-string result replacement)
      (buffer/push-byte result byte)))
  (string result))


(defn- lookup-fn
  ```
  Return a lookup function for a context `ctx`
  ```
  [ctx]
  (fn lookup [x]
    (var result nil)
    (def trimmed-x (string/trim x))
    (case trimmed-x
      "." # The implicit dot iterator
      (set result (array/peek ctx))

      (do # Regular lookup
        (def ks (->> trimmed-x (string/split ".") (map keyword)))
        (loop [i :down-to [(- (length ctx) 1) 0]]
          (when-let [val (get-in ctx [i ;ks])]
            (set result val)
            (break))
          (if (and (> (length ks) 1) (get-in ctx [i ;(slice ks 0 -2)]))
            (break)))))
    result))


(defn render
  ```
  Render the Mustache `template` using a dictionary `replacements`

  Musty will translate the Mustache template into a series of Janet expressions
  and then evaluate those expressions to produce a string.. The translation is
  accomplished by way of a parser expression grammar that matches particular
  tags and then causes the tag and its enclosed value to be replaced with the
  relevant Janet expression.

  Musty is a partial implementation of the Mustache specification. It supports
  variable tags, section tags, inverted section tags and comment tags.
  ```
  [template replacements]
  (def output
    (eval
     ~(fn [ctx]
        (let [lookup (,lookup-fn ctx)
              escape ,escape]
          ,;(peg/match mustache template)))))
  (output @[replacements]))
