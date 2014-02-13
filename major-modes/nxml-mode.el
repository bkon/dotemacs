;; nxml-mode

(setq nxml-slash-auto-complete-flag t)

(setq magic-mode-alist
      (cons '("<?xml " . nxml-mode)
            magic-mode-alist))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . nxml-mode))

(fset 'xml-mode 'nxml-mode)

(add-hook 'nxml-mode-hook 'linum-mode)
