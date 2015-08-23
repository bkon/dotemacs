;; jsx-mode.el
;; https://github.com/jsx/jsx-mode.el

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(setq-default jsx-mode-indent 2)
(add-hook 'jsx-mode-hook 'linum-mode)
