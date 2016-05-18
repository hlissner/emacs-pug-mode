# pug-mode

> This is a WIP. Use at your own risk! Contributions are welcome!

`pug-mode` offers Emacs support for [Pug](http://jade-lang.com/). Unlike `jade-mode`,
it is based off of [slim-mode](https://github.com/slim-template/emacs-slim).

## Why not use jade-mode?

I created pug-mode because `jade-mode` (based on `sws-mode`) is incomplete in some
annoying ways (to me). For example, it has poor/no fontification of plain-text, filter
blocks, or mixin definitions/invokations; I was also unhappy with its indentation
strategies and there wasn't enough time to grok and fix those issues.

In contrast, it took me 10 minutes to grok `slim-mode`'s source and another 10 to adapt
it to Jade in—what I think—is a better result. Most of the code is lifted right out of
slim-mode, so much of the credit belongs to its original developers!

_Note: I use Jade as a preprocessor rather than a template engine._

Maybe when I have more time, I'll look into merging these fixes into `jade-mode`.

