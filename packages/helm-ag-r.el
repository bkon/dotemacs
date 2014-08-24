;; helm-ag-r
;; https://github.com/yuutayamada/helm-ag-r

(global-set-key (kbd "C-x a a") 'helm-ag-r-from-git-repo)
(global-set-key (kbd "C-x a d") 'helm-ag-r)
(setq helm-ag-r-input-idle-delay nil)
