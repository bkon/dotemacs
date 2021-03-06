;; helm
;; https://github.com/emacs-helm/helm

(helm-mode)
(helm-adaptive-mode)

(global-set-key (kbd "C-x a b") 'helm-buffers-list)
(global-set-key (kbd "C-x a r") 'helm-pp-bookmarks)
(global-set-key (kbd "C-x a T") 'helm-simple-call-tree)
(global-set-key (kbd "C-x a o") 'helm-colors)
(global-set-key (kbd "C-x a u") 'helm-ucs)
(global-set-key (kbd "C-x a e") 'helm-do-grep)
(global-set-key (kbd "C-x a h") 'helm-info-at-point)
(global-set-key (kbd "C-x a H") 'helm-man-woman)
(global-set-key (kbd "C-x a l") 'helm-locate)
(global-set-key (kbd "C-x a SPC") 'helm-global-mark-ring)
(global-set-key (kbd "C-x a r") 'helm-register)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(define-key helm-find-files-map (kbd "C-.") 'helm-find-files-up-one-level)

;; Disable autoexpansion in find-files by default
(setq helm-ff-auto-update-initial-value nil)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))
