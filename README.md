![Release tag](https://img.shields.io/github/tag/hlissner/emacs-pug-mode.svg?label=release&style=flat-square)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/pug-mode-badge.svg)](http://melpa.org/#/pug-mode)
[![MELPA Stable](http://stable.melpa.org/packages/pug-mode-badge.svg)](http://stable.melpa.org/#/pug-mode)
[![Build Status](https://travis-ci.org/hlissner/emacs-pug-mode.png?branch=master&style=flat-square)](https://travis-ci.org/hlissner/emacs-pug-mode)

# pug-mode

`pug-mode` offers Emacs support for [Pug](https://pugjs.org/). Unlike
`jade-mode`, it is based off of
[slim-mode](https://github.com/slim-template/emacs-slim).

## Installation

`pug-mode` is available on MELPA.

`M-x package-install RET pug-mode`

```emacs-lisp
(require 'pug-mode)
```

## Why not use jade-mode?

I created pug-mode because [jade-mode](https://github.com/brianc/jade-mode)
(based on `sws-mode`) is incomplete in ways that are inconvenient to me. For
instance, it has poor/no fontification of plain-text, filter blocks, or mixin
definitions/invokations; its indentation strategies seemed too aggressive and
the source was too difficult to grok quickly to address these issues.

In contrast, it took me 10 minutes to grok `slim-mode`'s source and another 10
to adapt it to Pug in—what I think—is a better result. Most of the code is
lifted right out of slim-mode, so much of the credit belongs to its original
developers!

## Auto-compiling pug files

This plugin introduces a `pug-compile` function. You can call it
directly (e.g. `M-x pug-compile`) or have it done automatically for .pug files:

```emacs-lisp
(defun pug-compile-saved-file()
  (when (and (stringp buffer-file-name)
             (string-match "\\.pug\\'" buffer-file-name))
     (pug-compile)))
(add-hook 'after-save-hook 'pug-compile-saved-file)
```

It requires [pug-cli](https://www.npmjs.com/package/pug-cli).

## Contributions

I am no expert at writing major modes for Emacs and welcome contributions of any
kind. Be they pull requests, bug reports or elisp tips!
