;; makes use of ef-theme colours in the modeline
(require 'ef-themes)

(defvar my-modeline--default-mode-line mode-line-format
  "A copy of the previous value of `mode-line-format'.")

(defun my-modeline-update ()
  "Update style of the modeline faces to match the choice of ef-theme."
  (ef-themes-with-colors
    (custom-set-faces
     `(mode-line ((,c :background ,bg-mode-line :height 120
                      :foreground ,fg-main :box (:line-width (1 . 6) :color ,bg-mode-line))))
     `(mode-line-inactive ((,c :background ,bg-alt :box (:line-width (1 . 1) :color ,fg-dim)))))))

;; There are four ways to specify text properties for text in the mode line:
;;
;;    1. Put a string with a text property directly into the mode line
;;    data structure, but see The Data Structure of the Mode Line for
;;    caveats for that.
;;
;;    2. Put a text property on a mode line %-construct such as
;;    ‘%12b’; then the expansion of the %-construct will have that
;;    same text property.
;;
;;    3. Use a (:propertize elt props…) construct to give elt a text
;;    property specified by props.
;;
;;    4. Use a list containing :eval form in the mode line data
;;    structure, and make form evaluate to a string that has a text
;;    property.
;;
(setq-default my-modeline-format
              '(
		"%e" mode-line-front-space
		;; e.g. fire symbol below for unsaved buffer is selected via (C-x 8 RET)
                (:eval (if (buffer-modified-p)
                           (propertize "🔥 " 'face 'error)
                         (propertize "- " 'face 'shadow)
                         )
                       )
                ;; if file-truename is "~/a/b/../c/d/filename" then show "a/b/../c/d" in darker colour
                (:eval (if buffer-file-name  ; not all buffers have a filename (e.g. messages/scratch)
                           (when (mode-line-window-selected-p) 
                             (propertize 
                              (string-join (seq-subseq (split-string buffer-file-truename "/") 1 -1) "/") 
                              'face 'shadow)                                      
                             ) 
                         ) 
                       )
                ;; ALWAYS show the final filename even if inactive
                ;; final separator is in usual font
                "/"
                ;; filename in a more obvious (warning) colour
                (:eval (if buffer-file-name  ; not all buffers have a filename (e.g. messages/scratch)
                           (propertize 
                            (string-join (seq-subseq (split-string buffer-file-truename "/") -1 nil)) 
                            'face 'warning)
                         )
                       )
                ;; everything after here goes on the right. This
                ;; doesn' work for emacs 29 ... needs emacs 30+?
                ;; mode-line-format-right-align
                (:eval (propertize " | " 'face 'shadow) ) ; separator
                ;; there is a default string for the modeline from the mu4e package
                (:eval (mu4e--modeline-string))
                ;; show ONLY the major mode (minor modes are not shown)
                (:eval (propertize " | " 'face 'shadow) ) ; separator
                ;; strip "-Mode" from the end
                (:eval (when (mode-line-window-selected-p) 
                         (propertize (nth 0
                                          (split-string
                                           (capitalize (symbol-name major-mode)) "-Mode")
                                          )
                                     'face 'success)
                         )
                       )
                " "
                (vc-mode vc-mode)
                (:eval (propertize " | " 'face 'shadow) ) ; separator
                mode-line-position        ; show lines and columns as specified above
                )
              )

(define-minor-mode my-modeline-mode
  "Minor mode to set the mode line."
  :init-value nil
  :keymap nil
  :lighter "my-modeline"
  :global t
  (if my-modeline-mode
      (progn
        ;; Set the new mode-line-format
        (setq-default mode-line-format my-modeline-format))
    (progn
      ;; Restore the original mode-line format
      (setq-default mode-line-format my-modeline--default-mode-line))))

(provide 'my-modeline)
