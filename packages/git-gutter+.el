;; git-gutter+
;; https://github.com/nonsequitur/git-gutter-plus

(eval-after-load "git-gutter+"
  (lambda ()
    (define-key git-gutter+-mode-map (kbd "C-x g n") 'git-gutter+-next-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x g p") 'git-gutter+-previous-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x g v") 'git-gutter+-show-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x g r") 'git-gutter+-revert-hunks)
    (define-key git-gutter+-mode-map (kbd "C-x g s") 'git-gutter+-stage-hunks)
    (define-key git-gutter+-mode-map (kbd "C-x g c") 'git-gutter+-commit)
    (define-key git-gutter+-mode-map (kbd "C-x g C") 'git-gutter+-stage-and-commit)))

;; Enable GitGutter by default
(global-git-gutter+-mode)
