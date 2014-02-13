;; yasnippet
;; https://github.com/capitaomorte/yasnippet

(yas-global-mode 1)

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-no-prompt))

;; Simple yas helper function
(defun uncapitalize (str)
  (concat
   (downcase (substring str 0 1))
   (substring str 1)  ))

;; Use yasnippet for file skeletons
(defun autoinsert-yas-expand()
  (progn
    (interactive)
    (yas-expand)
    ))
