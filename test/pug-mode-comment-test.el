;;; test/pug-mode-comment-test.el -*- lexical-binding: t; -*-

(ert-deftest pug-mode-comment-inline-block ()
  ""
  (equal!
      ("p Hello world")
      ("//-" "  p Hello world")
    (pug-comment-block))
  (equal!
      ("ul"
       "  li item 1"
       "  li item 2{1}"
       "  li item 3")
      ("ul"
       "  li item 1"
       "  //-"
       "    li item 2"
       "  li item 3")
    (goto-char! "1")
    (pug-comment-block)))

(ert-deftest pug-mode-comment-multiline-block ()
  ""
  (equal!
      ("p."
       "  Hello, world"
       "  Foo, bar{1}"
       "  Fizz, buzz")
      ("p."
       "  Hello, world"
       "  //-"
       "    Foo, bar"
       "  Fizz, buzz")
    (goto-char! "1")
    (pug-comment-block)))

(provide 'pug-mode-comment-test)
;;; pug-mode-comment-test.el ends here
