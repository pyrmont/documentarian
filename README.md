# Documentarian

Documentarian is a minimal documentation generation tool for Janet projects.

Documentarian makes it easy to take the docstrings you've already written for
your code and turn them into a simple Markdown-formatted document. This document
can be included in your repository and read easily on GitHub.

## Requirements

Documentarian requires Janet 1.9.0 or higher.

Documentarian also depends on your project having a `project.janet` file that
contains a `:name` key in the `declare-project` form and a `:source` key in the
`declare-source` form. The `:source` key can be associated with individual
Janet files or a directory containing Janet files.

Because the API document is generated in Markdown, you can include Markdown in
your docstrings. Don't forget that Janet supports `` ` ``-delimited
[long strings][ls] in addition to regular `"`-delimited strings. Long strings
preserve whitespace (including newlines) which can be used to create lists,
headings and code blocks.

[ls]: https://janet-lang.org/docs/strings.html

## Building

Clone the repository and then run:

```console
$ jpm build
```

The `documentarian` binary is in the `build` directory.

## Usage

If you have your code in a directory `src` at the top of your project, you can
run Documentarian like so:

```console
$ /path/to/documentarian
```

Your documentation will be in a file called `api.md`.

### Command-Line Arguments

Documentarian supports the following command-line arguments:

```
 Optional:
 -d, --defix VALUE=src                       Remove a directory name from function names.
 -e, --echo                                  Prints output to stdout.
 -h, --help                                  Show this help message.
 -i, --input VALUE=project.janet             Specify the project file.
 -o, --output VALUE=api.md                   Specify the output file.
 -p, --private                               Include private values.
```

## Bugs

Found a bug? I'd love to know about it. The best way is to report your bug in
the [Issues][] section on GitHub.

[Issues]: https://github.com/pyrmont/documentarian/issues

## Licence

Documentarian is licensed under the MIT Licence. See [LICENSE][] for more
details.

[LICENSE]: https://github.com/pyrmont/documentarian/blob/master/LICENSE
