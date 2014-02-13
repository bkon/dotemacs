;; zencoding-mode
;; https://github.com/rooney/zencoding

(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'nxml-mode-hook 'zencoding-mode)

(defun zencoding-indent-size ()
  (let ((mode (with-current-buffer (current-buffer) major-mode)))
    (cond
     ((string= mode "nxml-mode") nxml-child-indent)
     ((string= mode "html-mode") sgml-basic-offset)
     ((string= mode "sgml-mode") sgml-basic-offset)
     (t 4)
     ))
  )

(defun zencoding-indent (text)
  "Indent the text"
  (if text
      (replace-regexp-in-string
       "\n"
       (concat
        "\n"
        (make-string (zencoding-indent-size) ?\ ))
       (concat "\n" text))
    nil))
