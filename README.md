[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/pug-mode-badge.svg)](http://melpa.org/#/pug-mode)
[![MELPA Stable](http://stable.melpa.org/packages/pug-mode-badge.svg)](http://stable.melpa.org/#/pug-mode)

# pug-mode

`pug-mode` offers Emacs support for [Pug](http://jade-lang.com/). Unlike
`jade-mode`, it is based off of
[slim-mode](https://github.com/slim-template/emacs-slim).

## Installation

`pug-mode` is available on MELPA.

`M-x package-install RET pug-mode`

Then a `(require 'pug-mode)` will do. No additional setup required!

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
directly (e.g. `M-x pug-compile`) or have it done automatically:

`(add-hook 'after-save-hook 'pug-compile)`

It requires [pug-cli](https://www.npmjs.com/package/pug-cli).

## TODO

+ Define faces to improve customizability
+ Use multiline-font-lock
+ Optimize pug-indent-p and font-lock regexes
+ Refactor out vestigial slim-mode code

## Contributions

I'm inexperienced at writing major modes for Emacs. Any contributions are
welcome and appreciated!
