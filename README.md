# pug-mode

`pug-mode` offers Emacs support for [Pug](http://jade-lang.com/). Unlike `jade-mode`,
it is based off of [slim-mode](https://github.com/slim-template/emacs-slim).

## Installation

`pug-mode` isn't available on MELPA, and won't be until it has been developed and tested
more comprehensively.

In the meantime, download `pug-mode.el` and insert the following into your emacs.d:

```elisp
(require 'pug-mode)
```

## Why not use jade-mode?

I created pug-mode because `jade-mode` (based on `sws-mode`) is incomplete in ways that
are inconvenient to me. For instance, it has poor/no fontification of plain-text, filter
blocks, or mixin definitions/invokations; its indentation strategies seemed too
aggressive and the source was too difficult to grok quickly to address these issues.

In contrast, it took me 10 minutes to grok `slim-mode`'s source and another 10 to adapt
it to Pug in—what I think—is a better result. Most of the code is lifted right out of
slim-mode, so much of the credit belongs to its original developers!
