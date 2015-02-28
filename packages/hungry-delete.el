;; hungry-delete
;; https://github.com/soutaro/hungry-delete.el

(require 'hungry-delete)

(defun bkon/dwim-backward-kill-word (arg)
  (interactive "p")
  (let ((previous-char-is-space-or-newline
         (member (char-before) '(?\n ?\s))
         ))
    (if previous-char-is-space-or-newline
        (hungry-delete-backward arg nil)
      (backward-kill-word arg))))

(global-set-key (kbd "<C-backspace>") 'bkon/dwim-backward-kill-word)
