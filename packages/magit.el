;; magit
;; https://github.com/magit/magit

(setq-default magit-diff-refine-hunk t
              magit-set-upstream-on-push t
              magit-status-buffer-switch-function 'switch-to-buffer
              magit-save-some-buffers nil)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

;; @FIXME
;; (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

(defun bkon/magit/add-branch-to-commit-message (&rest discard)
  "Add a current branch name to the default commit message"
  (unless current-prefix-arg ;; ignore commit amends
    (let ((tag (format "[%s] " (magit-get-current-branch))))
      (goto-char (point-min))
      (unless (search-forward tag nil t) (insert tag)))))
(add-hook 'git-commit-mode-hook 'bkon/magit/add-branch-to-commit-message)

(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x m m") 'magit-status)
(global-set-key (kbd "C-x m b") 'magit-blame-mode)
(global-set-key (kbd "C-x m l") 'magit-file-log)
