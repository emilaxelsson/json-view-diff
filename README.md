# `json-view-diff`

A tool for showing differences between JSON files.

The tool is mostly intended for personal use. I tried the following tools beforehand:

  * [aeson-diff](https://hackage.haskell.org/package/aeson-diff)
  * [jsondiff](https://pypi.org/project/jsonpatch)
  * [semantic](https://github.com/github/semantic)

Neither of these generates a nice, human-readable, colorized result. Some of them were quite slow on large files (presumably because they're computing minimal edit distances).

`json-view-diff` has the following features:

  * Generates an easily readable diff using colors on the terminal.
  * Shows pretty-printed output, even if the input doesn't have a pretty layout.
  * Only the parts of the data that differ are shown in the output.
  * Supports approximate equality on numbers (can be turned off using the `--exact` option).
  * Optionally supports files containing a list of single-line JSON blobs (which means the file isn't valid JSON).

One limitation (or feature) is that `json-view-diff` only does structural diffing. It does not attempt to compute minimal edit sequences. Lists are compared element by element, in order.

Note that the output doesn't strictly use JSON notation. Even ignoring the diff markers, the syntax is a bit different. This is a technical artifact of the [dino library](http://hackage.haskell.org/package/dino), which is used under the hood.
