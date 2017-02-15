;; typescript-mode
;; https://github.com/ananthakumaran/typescript.el

(setq typescript-indent-level 2)
(setq typescript-auto-indent-flag nil)
(add-to-list 'auto-mode-alist '(".tsx" . typescript-mode))
(add-hook 'typescript-mode-hook 'linum-mode)
