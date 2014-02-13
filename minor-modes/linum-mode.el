;; linum-mode

;; Line numbers are displayed in:

;; Line  numbers mode  customization:  numbers  are right-aligned  and
;; width of the linum column is calculated automatically
;;
;; Note:  whitespace-mode highlights  spaces in  margin, so  there's a
;; workaround for this: invisible leading zeros

(eval-after-load 'linum
  '(progn
     ;; Make leading zeros invisible (fg = bg)
     (defface linum-leading-zero
       `((t :inherit 'linum
            :foreground ,(face-attribute 'linum :background nil t)))
       "Face for displaying leading zeroes for line numbers in display margin."
       :group 'linum)

     ;; Pad line number with zeros using :linum-leading-zero face
     (defun linum-format-func (line)
       (let ((w
              (max
               4
               (1+
                (length
                 (number-to-string (count-lines (point-min) (point-max))))))))
         (concat
          ;; leading zeros
          (propertize (make-string (- w (length (number-to-string line))) ?0)
                      'face 'linum-leading-zero)
          ;; formatted number
          (propertize (number-to-string line) 'face 'linum))))

     (setq linum-format 'linum-format-func)))
