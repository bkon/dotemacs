;; projectile.el
;; https://github.com/bbatsov/projectile

(projectile-global-mode)

(global-set-key (kbd "C-x a a") 'ag-project)

(setq projectile-completion-system 'grizzl)
(setq projectile-switch-project-action 'projectile-dired)
