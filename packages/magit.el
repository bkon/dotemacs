;; magit
;; https://github.com/magit/magit

(setq-default magit-diff-refine-hunk t
              magit-set-upstream-on-push t
              magit-status-buffer-switch-function 'switch-to-buffer)

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
