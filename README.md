# Documentarian

Documentarian is a minimal documentation generation tool for Janet projects.

Documentarian makes it easy to take the docstrings you've already written for
your code and turn them into a simple Markdown-formatted document. This document
can be included in your repository and read easily on GitHub.

## Prerequisites

Documentarian depends on your project having a `project.janet` file that
contains a `:name` key in the `declare-project` section and a `:source` key in
the `declare-source` section. The `:source` key can be associated with
individual Janet files or a directory containing Janet files.

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

Documentarian generates a Markdown-formatted file, `api.md`. From the root of
your Janet project, run:

```console
$ /path/to/documentarian
```

By default, Documentarian will not create documentation for elements marked
private in your source code. You can override this with the `-p` switch.

## Bugs

Found a bug? I'd love to know about it. The best way is to report your bug in
the [Issues][] section on GitHub.

[Issues]: https://github.com/pyrmont/documentarian/issues

## Licence

Documentarian is licensed under the MIT Licence. See [LICENSE][] for more
details.

[LICENSE]: https://github.com/pyrmont/documentarian/blob/master/LICENSE
