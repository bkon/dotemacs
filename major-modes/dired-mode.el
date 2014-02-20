;; dired-mode

(defun bkon/dired-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun bkon/dired-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map [remap end-of-buffer] 'bkon/dired-bottom)
(define-key dired-mode-map [remap beginning-of-buffer] 'bkon/dired-top)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
