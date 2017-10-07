
(require 'ert)
(require 'pug-mode)

(setq-default
 pug-tab-width 2
 indent-tabs-mode nil)

;;
(defmacro equal! (source expected &rest body)
  "TODO"
  (declare (indent 2))
  `(with-temp-buffer
     (cl-loop for line in ',source
              do (insert line "\n"))
     (goto-char (point-min))
     (save-excursion
       (let (marker-list)
         (while (re-search-forward "{\\([0-9]+\\)}" nil t)
           (push (cons (match-string 1)
                       (set-marker (make-marker) (match-beginning 0)))
                 marker-list)
           (replace-match "" t t))
         (pug-mode)
         (goto-char (point-min))
         ,@body))
     (let ((result-text (buffer-string))
           expected-text)
       (with-temp-buffer
         (cl-loop for line in ',expected
                  do (insert line "\n"))
         (setq expected-text (buffer-string))
         (should (equal expected-text result-text))))))

(defmacro goto-char! (index)
  `(goto-char (point! ,index)))

(defmacro point! (index)
  `(cdr (assoc ,index marker-list)))
