(setq inhibit-startup-message t)

;; == Generic appearance

;; Maximized
(setq-default initial-frame-alist (quote ((fullscreen . maximized))))

;; Turn off toolbar
(tool-bar-mode -1)

;; Turn off menu bar
(menu-bar-mode -1)

;; Hide scrollbars
(scroll-bar-mode -1)

;; Hide left fringe (vertical line along  the side of the frame), keep
;; right fringe
(fringe-mode '(0 . 10))

;; set  left margin  width instead  of left  fringe, so  content won't
;; stick to the left edge without line numbers; linum mode resets left
;; margin
(setq-default left-margin-width 1)

(require 'cl)

(defun load-config (package base-dir error-message)
  (let ((filename (expand-file-name
                   (format
                    "%s/%s.el"
                    base-dir
                    (symbol-name package))
                   (file-name-directory load-file-name))))
    (if
        (file-exists-p filename)
        (load filename)
      (display-warning :warning (format error-message filename))
      )))

(defun load-package-config (package)
  (load-config package
               "packages"
               "Package configuration file is missing: %s"))

(defun load-major-mode-config (package)
  (load-config package
               "major-modes"
               "Major mode configuration file is missing: %s"))

(defun load-minor-mode-config (package)
  (load-config package
               "minor-modes"
               "Minor mode configuration file is missing: %s"))

;; MELPA package archive
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(let ((packages '(
                  zenburn-theme
                  ag
                  anzu
                  ac-ispell
                  ac-js2
                  auto-complete
                  auto-complete-nxml
                  bookmark+ ;; todo
                  bundler
                  butler
                  coffee-mode
                  cursor-chg
                  dired+ ;; todo
                  diminish
                  dired-details
                  dtrt-indent
                  expand-region
                  feature-mode
                  fic-mode
                  fill-column-indicator
                  flymake-coffee
                  flymake-css
                  flymake-cursor
                  flymake-haml
                  flymake-jshint
                  flymake-jslint
                  flymake-json
                  flymake-php
                  flymake-phpcs
                  flymake-puppet
                  flymake-ruby
                  flymake-sass
                  flymake-shell
                  flymake-yaml
                  flyspell-lazy
                  frame-restore
                  fuzzy
                  gist ;; todo
                  git-gutter+
                  git-gutter-fringe+
                  git-messenger
                  graphviz-dot-mode
                  haml-mode
                  helm
                  helm-css-scss
                  helm-flymake
                  helm-git-grep
                  helm-projectile
                  helm-swoop
                  highlight-indentation
                  js2-mode
                  js2-refactor ;; todo
                  json-mode ;; todo
                  magit ;; todo
                  markdown-mode ;; todo
                  mmm-mode ;; todo
                  nginx-mode ;; todo
                  page-break-lines
                  php-mode
                  popup
                  popwin ;; todo
                  powerline ;; todo
                  projectile ;; todo
                  puppet-mode ;; todo
                  rainbow-delimiters
                  rainbow-mode
                  revive
                  rinari ;; todo
                  robe ;; todo
                  rspec-mode ;; todo
                  sass-mode ;; todo
                  scss-mode ;; todo
                  simple-call-tree ;; todo
                  smart-mode-line ;; todo
                  smart-operator ;; todo
                  sql-indent ;; todo
                  syslog-mode ;; todo
                  visual-regexp ;; todo
                  visual-regexp-steroids ;; todo
                  vline ;; todo
                  yaml-mode
                  yard-mode
                  yasnippet ;; todo
                  zencoding-mode ;; todo
                  )))

  (unless (every #'package-installed-p packages)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (mapc #'(lambda (package)
              (unless (package-installed-p package)
                (package-install package)))
          packages))
  (mapc #'load-package-config packages))

(mapc #'load-major-mode-config
      '(
        css-mode
        dired-mode
        emacs-lisp-mode
        eshell-mode
        help-mode
        html-mode
        js-mode
        nxml-mode
        ruby-mode
        sh-mode
        ))

(mapc #'load-minor-mode-config
      '(
        auto-insert-mode
        column-number-mode
        desktop-save-mode
        electric-pair-mode
        hl-line-mode
        line-number-mode
        linum-mode
        recentf-mode
        show-paren-mode
        size-indication-mode
        subword-mode
        which-func-mode
        whitespace-mode
        ))

;; == Paths and tools

;; Setting rbenv path
(setenv "PATH"
        (concat
         (getenv "HOME") "/.rbenv/shims:"
         (getenv "HOME") "/.rbenv/bin:"
         (getenv "PATH")))
(setq exec-path
      (cons
       (concat (getenv "HOME") "/.rbenv/shims")
       (cons
        (concat (getenv "HOME") "/.rbenv/bin")
        exec-path)))

;; == Key bindings

;; Move between windows using Shift + Arrows
(windmove-default-keybindings)

;; Hippie expand
(global-set-key [C-tab] 'hippie-expand)

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Indent on pressing RET (instead of just newline)
(define-key global-map (kbd "RET") 'newline-and-indent)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; @TODO
;; (global-set-key (kbd "<C-O>") 'open-line-below)
;; (global-set-key (kbd "<C-S-return>") 'open-line-above)

;; == Global behavior

;; You can replace the active region  just by typing text, and you can
;; delete the selected text just by hitting the Backspace key
(delete-selection-mode 1)

;; Enable several advanced features which are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Default coding system
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Disable backups
(setq backup-inhibited t)

;; Disable auto save
(setq auto-save-default nil)

;; Midnight mode (cleans up old unused buffers)
(setq midnight-mode t)

;; Scroll by 1 line instead of jumping around
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t ; rename after killing
      uniquify-ignore-buffers-re "^\\*")

;; Use simple y/n prompt
(fset 'yes-or-no-p 'y-or-n-p)

;; == Session management

;; Save buffer position when file is closed and reopened within the same
;; session; save-desktop does this between different sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file
      (expand-file-name
       ".places"
       (concat (file-name-as-directory user-emacs-directory)
               "run")
      ))


;; Show keystrokes being typed almost immediately
(setq echo-keystrokes 0.1)

;; == Editor behavior

;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; Make M-q treat bulleted lists (starting with -*) as paragraphs
(setq-default paragraph-start "\f\\|[ \t]*$\\|[-*] +.+$"
              paragraph-separate "$")

;; Methods to use when doing hippie expand
(setq-default hippie-expand-try-functions-list
              '(yas-hippie-try-expand ;; YASnippet expansion is tried first
                try-complete-file-name-partially
                try-complete-file-name
                try-expand-all-abbrevs
                try-expand-list
                try-expand-line
                try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol))

;; == Flyspell

(require 'flyspell)

(setq-default ispell-program-name "aspell"
              ispell-list-command "--list"
              ispell-extra-args '("--sug-mode=ultra"))

(require 'flymake)

(setq tramp-default-method "ssh")

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; == Automated customizations

;; Custom  line  number face  (linum):  slightly  smaller size,  fixed
;; normal weight
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:height 0.8 :weight normal))))
 '(show-paren-match ((t (:background "OliveDrab4" :foreground "black" :weight bold))))
 '(show-paren-mismatch ((t (:background "firebrick" :foreground "black" :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-auto-light-when-jump (quote any-bookmark))
 '(bmkp-auto-light-when-set (quote any-bookmark))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(safe-local-variable-values (quote ((js-indent-level . 2)))))

;; Key prefixes:
;; C-x
;;   tab - indent
;;   q - macro query
;;   w - free
;;   e - kmacro-end-and-call-macro
;;   r - register / rectangle commands
;;   t - free
;;   y - free
;;   u - free (undo)
;;   i - insert file
;;   o - other window
;;   p - bookmarks
;;   [ - backward page
;;   ] - forward page
;;   \ - free
;;   a - helm
;;   s - save
;;   d - dired
;;   f - free (set-fill-column, mostly unused)
;;   g - git-gutter+
;;   h - mark whole buffer
;;   j - bookmark jump
;;   k - kill buffer
;;   l - free (number of lines)
;;   ; - free (comment column)
;;   ' - free (expand abbrev)
;;   z - repeat
;;   x - free
;;   c - free (helm command to be moved to a) use for jenkins (*C*I)?
;;   v - free (vc commands; I use magit)
;;   b - free (unused buffer nav) use for *b*undler?
;;   n - narrow
;;   m - free (mail)
;;   , - free
;;   . - free? (set-fill-prefix)
;;   / - free

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
