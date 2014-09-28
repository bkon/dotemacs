;; projectile.el
;; https://github.com/bbatsov/projectile

(projectile-global-mode)

(global-set-key (kbd "C-x a a") 'ag-project)

(defun projectile-magit ()
  "Open magit for the current project"
  (interactive)
  (magit-status (projectile-project-root))
  )

(setq projectile-completion-system 'grizzl)
(setq projectile-switch-project-action 'projectile-magit)
