;; makes use of ef-theme colours in the modeline
(require 'ef-themes)
(require 'ryo-modal)

;(defgroup rh-ef-modeline nil
;  "My slightly customise mode line."
;  :prefix "rh-ef-modeline-"
;  :group 'mode-line)

(defvar rh-ef-modeline--default-mode-line mode-line-format
  "A copy of the previous value of `mode-line-format'.")

(defun rh-ef-modeline-update ()
  "Update style of the modeline faces to match the choice of ef-theme"
  (ef-themes-with-colors
    (custom-set-faces
     `(mode-line ((,c :background ,bg-mode-line 
                      :foreground ,fg-main :box (:line-width 4 :color ,bg-mode-line))))
     `(mode-line-inactive ((,c :background ,bg-inactive :box (:line-width 4 :color ,bg-inactive)))))))

;; this is not a 'package' for general use, so I simply hardwire the
;; default modeline to suite my preferences
;; e.g. fire symbol below for unsaved buffer is selected via (C-x 8 RET)
(setq-default rh-ef-modeline-format
              '(
                (:eval (cond
                        (ryo-modal-mode
                         (propertize " ♌ " 'face 'error)) ;; obvious modal indicator
                        (t
                         (propertize " - " 'face 'shadow))))
                (:eval (if (buffer-modified-p)
                           (propertize " 🔥   " 'face 'error)
                         (propertize " -    " 'face 'shadow)
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
                (:eval (propertize "   |   " 'face 'shadow) ) ; separator
                ;; there is a default string for the modeline from the mu4e package
                (:eval (mu4e--modeline-string))
                (:eval (when (mode-line-window-selected-p) 
                         (if (buffer-live-p (get-buffer "*mu4e-main*"))
                             " 📫"
                           " . ")))
                ;; show ONLY the major mode (minor modes are not shown)
                (:eval (propertize "   |   " 'face 'shadow) ) ; separator
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
                (:eval (propertize "   |   " 'face 'shadow) ) ; separator
                mode-line-position        ; show lines and columns as specified above
                )
              )

(define-minor-mode rh-ef-modeline-mode
  "Minor mode to set the mode line."
  :init-value nil
  :keymap nil
  :lighter "rh-ef-modeline"
  :global t
  (if rh-ef-modeline-mode
      (progn
        ;; Set the new mode-line-format
        (setq-default mode-line-format rh-ef-modeline-format))
    (progn
      ;; Restore the original mode-line format
      (setq-default mode-line-format rh-ef-modeline--default-mode-line))))

(provide 'rh-ef-modeline)
