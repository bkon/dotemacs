;; auto-complete
;; http://cx4a.org/software/auto-complete/

(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(setq-default ac-auto-show-menu t)
(setq-default ac-use-menu-map t)
(setq-default ac-use-fuzzy t)
(setq-default ac-use-comphist t)
(setq-default ac-quickhelp-delay 0.2)

;; Ignore case if completion target string doesn't include upper characters
(setq-default ac-ignore-case 'smart)

;; Popup mode:
;; C-s - start filtering
;; DEL / C-h - clear filter

(global-set-key [C-S-iso-lefttab] 'auto-complete)
(global-set-key (kbd "C-/") 'auto-complete)

(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(global-auto-complete-mode 1)

(add-hook 'css-mode-hook 'ac-css-mode-setup)
