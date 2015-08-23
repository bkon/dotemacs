;; magit
;; https://github.com/magit/magit

(setq-default magit-diff-refine-hunk t
              magit-set-upstream-on-push t
              magit-status-buffer-switch-function 'switch-to-buffer
              magit-save-some-buffers nil)

(defun bkon/magit/add-branch-to-commit-message (&rest discard)
  "Add a current branch name to the default commit message"
  (unless current-prefix-arg ;; ignore commit amends
    (if (magit-get-current-branch)
        (let ((tag (format "[%s] " (magit-get-current-branch))))
          (goto-char (point-min))
          (unless (search-forward tag nil t) (insert tag))))))

(defun bkon/magit/add-newline-to-commit-message (&rest discard)
  "Add newline to a commit message, so magit won't complain about the second line"
  (unless current-prefix-arg ;; ignore commit amends
    (search-forward "#")
    (backward-char)
    (insert "\n")))


(add-hook 'git-commit-mode-hook 'bkon/magit/add-branch-to-commit-message)
(add-hook 'git-commit-mode-hook 'bkon/magit/add-newline-to-commit-message)

(eval-after-load 'magit-mode
  '(define-key magit-mode-map (kbd "C-x a") nil))

(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x m m") 'magit-status)
(global-set-key (kbd "C-x m b") 'magit-blame-mode)
(global-set-key (kbd "C-x m l") 'magit-file-log)
