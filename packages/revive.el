;; revive
;; https://github.com/emacsmirror/rainbow-mode

;; revive hooks for automatically saving and restoring window configuration
(add-hook 'kill-emacs-hook 'save-current-configuration)
(add-hook 'after-init-hook 'resume)
