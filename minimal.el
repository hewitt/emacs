(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities ; prefer ELPA to MELPA
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5 )))
(package-initialize)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; add a location for supporting elisp code
(add-to-list 'load-path "~/.emacs.d/elisp")

;; skip auto backups
(setq make-backup-files nil)
;; backups can all be pushed to a particular directory if needed
;;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; don't show the default startup screen
(setq inhibit-startup-screen t)
;; don't end sentences with a double space
(setq sentence-end-double-space nil)
;; the frequency of garbage collection
(setq gc-cons-threshold 8000000 ) ; i.e., every ~8MB
;; report GC events
(setq garbage-collection-messages t)
;; warn when opening files bigger than 80MB
(setq large-file-warning-threshold 80000000)
;; always follow the symlink
(setq vc-follow-symlinks t)

;; keep track of recently opened files
(recentf-mode 1)
;; have mouse input in the terminal -- the disadvantage is you
;; need to SHIFT+middle mouse to paste in the terminal
(xterm-mouse-mode 1)
;; Turn off the menu/scroll/toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; replace annoying yes/no with y/n
(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode)

;; use both line & column numbers
(setq mode-line-position (list "L%l C%c"))
;; this gets hooked later to update modeline colours when the theme is changed
(defun my-modeline-update ()
  "Update style of the modeline faces to match the choice of ef-theme."
  (ef-themes-with-colors
    (custom-set-faces
     `(mode-line ((,c :background ,bg-mode-line :height 120
                      :foreground ,fg-main :box (:line-width (1 . 6) :color ,bg-mode-line))))
     `(mode-line-inactive ((,c :background ,bg-alt :box (:line-width (1 . 1) :color ,fg-dim)))))))

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
                mode-line-format-right-align
                (:eval (propertize " | " 'face 'shadow) ) ; separator
                ;; there is a default string for the modeline from the mu4e package
                ;;(:eval (propertize (mu4e--modeline-string) 'face 'shadow))
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
		"  "
                )
              )

;; make the above definition the mode-line
(setq-default mode-line-format my-modeline-format)
;; apply the hook to keep modeline colours up to date with current theme
(add-hook 'ef-themes-post-load-hook #'my-modeline-update)

(use-package ef-themes
  :init
  ;; Disable all other themes to avoid awkward blending
  (mapc #'disable-theme custom-enabled-themes)
  (setq ef-themes-to-toggle '(ef-maris-dark ef-elea-light)))
(ef-themes-select 'ef-maris-dark)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(use-package rainbow-delimiters
  :init
  (message "Use-package: Rainbow delimiters")
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (latex-mode . rainbow-delimiters-mode))

(use-package which-key
  :init 
  (message "Use-package: Which-key mode")
  :config
  (setq which-key-idle-delay 0.25) 
  (setq max-mini-window-height 0.25) ; don't show bigger than 1/4 of the frame height
  (which-key-setup-minibuffer)       ; use the minibuffer to show help
  (which-key-mode))

(use-package fontaine)

(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))

(setq fontaine-presets
      '((bitmap
         :default-family "envypn"
         :default-weight regular
         :default-height 113
         :fixed-pitch-family nil
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family nil
         :variable-pitch-weight normal
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing 1)
        (iosevka
         :default-family "Iosevka"
         :default-weight normal
         :default-height 120
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family nil
         :variable-pitch-weight normal
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil ; use whatever the underlying face has
         :italic-slant italic
         :line-spacing 1)
        (terminus
         :default-family "Terminus"
         :default-weight normal
         :default-height 150
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family nil
         :variable-pitch-weight normal
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil ; use whatever the underlying face has
         :italic-slant italic
         :line-spacing 1)))

;; Persist the latest font preset when closing/starting Emacs and
;; while switching between themes.
                                        ;(fontaine-mode 1)

;; fontaine does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
(define-key global-map (kbd "C-c f") #'fontaine-set-preset)

(use-package consult
  :init
  (message "Use-package: consult")
  :bind
  ;; some standard emacs-chord bindings -- but see also evil section.
  ("C-x b" . consult-buffer)
  ("M-g g" . consult-goto-line)
  ("M-y"   . consult-yank-pop)
  ("C-y"   . yank)
  ("C-s"   . consult-line)
  ("M-g o" . consult-outline))

(use-package consult-notes
  :defer t
  :commands (consult-notes consult-notes-search-in-all-notes)
  :config
  (consult-notes-denote-mode))

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (message "Use-package: vertico")
  (vertico-mode))

(use-package prescient
  :init
  (message "Use-package: prescient")
  :config
  ;; you have to set the completion-style(s) to be used
  (setq completion-styles '(substring prescient basic))
  ;; retain completion statistics over restart of emacs
  (prescient-persist-mode))

(use-package vertico-prescient
  :init
  (message "Use-package: vertico-prescient")
  :config
  (vertico-prescient-mode))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (message "Use-package: marginalia")
  (marginalia-mode))

(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
                                        ; left/right occupies full window height
(setq window-sides-vertical t)                    
                                        ; pop new window if switching buffers from dedicated
(setq switch-to-buffer-in-dedicated-window 'pop)  
(setq split-height-threshold 80)
(setq split-width-threshold 120)
(setq window-min-height 5)
(setq window-min-width 90)

;;(setq display-buffer-alist 'nil) ; to remove all preferences
(setq display-buffer-alist
      `(
        ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"                 ; match all the usual capture buffers
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
                                        ;(window-parameters . ((mode-line-format . none)) ) ; turn off the mode line
         )
        ("\\*Org Agenda\\*"                                 ; always put my calendar and compose windows on the right
         (display-buffer-in-side-window)
         (dedicated . t)                                    ; don't reuse this buffer for other things
         (window-width . 120)
         (side . right)                                     ; put it on the right side
                                        ;(window-parameters . ((mode-line-format . none)))  ; turn off the mode line
         )	
        ((derived-mode . mu4e-compose-mode)                 ; always put my calendar and compose windows on the right
         (display-buffer-in-side-window)
         (dedicated . t)                                    ; don't reuse this buffer for other things
         (window-width . 120)
         (side . right)                                     ; put it on the right side
                                        ;(window-parameters . ((mode-line-format . none)))  ; turn off the mode line
         )	
        ((or (derived-mode . mu4e-headers-mode)
             (derived-mode . mu4e-main-mode ))              ; other mu4e stuff remains dedicated
         (display-buffer-reuse-mode-window)                 ; don't always open a new window
         (dedicated . t)                                    ; don't reuse this buffer for other things
         ;;(window-parameters . ((mode-line-format . none)))  ; turn off the mode line
         )
        ((derived-mode . pdf-view-mode)
         (display-buffer-in-side-window))
        ("\\*Org \\(Select\\|Note\\)\\*"                    ; put other Org stuff at the bottom
         (display-buffer-in-side-window)
         (dedicated . t)                                    ; don't reuse this buffer for other things
         (side . bottom)
         ;;(window-parameters . ((mode-line-format . none)))  ; turn off the mode line
         )          
        ))

;; move focus when splitting a window
(defun my/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'my/split-and-follow-horizontally)
;; move focus when splitting a window
(defun my/split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'my/split-and-follow-vertically)

;; short-cut to edit the init.el configuration file
(defun my/config-visit ()
  (interactive)
  (find-file "~/CURRENT/NixConfig/outOfStore/.emacs.d/config.org") )

;; short-cut to edit the init.el configuration file
(defun my/todo-visit ()
  (interactive)
  (find-file "~/Sync/Org/Todo.org") )

(defvar-keymap my-prefix-org-map
  :doc "Prefix map for Org mode."
  "c" #'org-capture
  "a" #'org-agenda
  "j" #'org-journal-new-entry
  "t" #'org-babel-tangle)

(defvar-keymap my-prefix-display-map
  :doc "Prefix map for display features."
  "+" #'text-scale-adjust
  "f" #'fontaine-set-preset)

;; Define a key map with commands and (potentially nested) key maps
(defvar-keymap my-prefix-map
  :doc "My prefix key map."
  "o" my-prefix-org-map
  "d" my-prefix-display-map
  "s" #'consult-notes-search-in-all-notes
  "t" #'my/todo-visit
  "e" #'my/config-visit
  "m" #'mu4e
  "f" #'dired
  )

;; Define how the nested keymaps are labelled in `which-key-mode'.
(which-key-add-keymap-based-replacements my-prefix-map
  "o" `("Org" . ,my-prefix-org-map)
  "d" `("display" . ,my-prefix-display-map)
  )

;; Bind the prefix key map to a key.  Notice the absence of a quote for
;; the map's symbol.
(keymap-set global-map "C-c r" my-prefix-map)

;; - cut and paste in Wayland environment
;; - this puts selected text into the Wayland clipboard
(setq x-select-enable-clipboard t)
(defun my/txt-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "wl-copy" ))
  )
(setq interprogram-cut-function 'my/txt-cut-function)

;; editorconfig allows local specification of tab/space/indent
;; using a config file in the directory
(use-package editorconfig
  :init
  (message "Use-package: EditorConfig")
  :config
  (editorconfig-mode 1) )

(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))

;; location of my snippets -- has to go before yas-reload-all
(setq-default yas-snippet-dirs '("~/.emacs.d/my_snippets"))
;; include yansippet and snippets
(use-package yasnippet
  :init
  (message "Use-package: YASnippet")
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; hooks for YASnippet in Latex, C++, elisp & org ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'c++-mode-hook 'yas-minor-mode)  
  (add-hook 'latex-mode-hook 'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  ;; remove default keybinding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; redefine my own key
  (define-key yas-minor-mode-map (kbd "M-<tab>") yas-maybe-expand)
  ;; remove default keys for navigation
  (define-key yas-keymap [(tab)]       nil)
  (define-key yas-keymap (kbd "TAB")   nil)
  (define-key yas-keymap [(shift tab)] nil)
  (define-key yas-keymap [backtab]     nil)
  ;; redefine my own keys
  (define-key yas-keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "M-p") 'yas-prev-field)  
  (yas-reload-all) )

(use-package transpose-frame)

;; GIT-GUTTER: SHOW changes relative to git repo
(use-package git-gutter
  :init
  (message "Use-package: Git-Gutter")
  :hook
  (prog-mode . git-gutter-mode)
  (org-mode . git-gutter-mode)
  (latex-mode . git-gutter-mode))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
;; maximum level of highlighting
(setq treesit-font-lock-level 4)

;; eglot is a simpler alternative to LSP-mode
(use-package eglot
  :init
  (message "Use-package: Eglot")
  ;; start eglot in my usual prog modes
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  ;; (add-hook 'latex-mode-hook 'eglot-ensure) 
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  :custom
  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("ccls")))
  ;;(add-to-list 'eglot-server-programs '(latex-mode . ("digestif"))) ; more annoying than helpful!
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))  )

(use-package direnv
  :config
  (direnv-mode))

(use-package pyvenv-auto
  :hook ((python-ts-mode . pyvenv-auto-run)))

(use-package highlight-indentation
  :after python
  :hook (python-ts-mode . highlight-indentation-mode)
  )

(use-package flymake-ruff
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package reformatter
  :hook 
  ; mostly "OK" but sometimes makes stupid formatting decisions
  (python-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))

;; (code) completion via in-buffer pop-up choices
(use-package corfu
  :init (message "Use-package: Corfu")
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (latex-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes' to exclude certain modes.
  :init
  ;;(setq tab-always-indent 'complete)
  (global-corfu-mode)
  (corfu-prescient-mode))

(use-package corfu-prescient
  :init
  (message "Use-package: corfu-prescient"))

;; my default gnuplot extension
(add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))
;; Octave/Matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; Nix language
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(use-package org
  :init
  (message "Use-package: Org") )

;; fancy replace of *** etc
(use-package org-bullets
  :after org
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (message "Use-package: Org-bullets") )

;; some appearance tweaks:
;;
;; replace emphasis with colors in Org files
(setq org-emphasis-alist
      '(("*" my/org-emphasis-bold)
        ("/" my/org-emphasis-italic)
        ("_" my/org-emphasis-underline)
        ("=" org-verbatim verbatim)
        ("~" org-code verbatim)
        ("+" (:strike-through t))))
;;
;; colorise text instead of changing the font weight.
(defface my/org-emphasis-bold
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff8059"))
  "My bold emphasis for Org.")
;;
(defface my/org-emphasis-italic
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#005e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#44bc44"))
  "My italic emphasis for Org.")
;;
(defface my/org-emphasis-underline
  '((default :inherit underline)
    (((class color) (min-colors 88) (background light))
     :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#d0bc00"))
  "My underline emphasis for Org.")

;; custom capture
(require 'org-capture)
;;(define-key global-map "\C-cc" 'org-capture) ; defined via ryo-modal
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/Sync/Org/Todo.org" "Inbox")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("z" "Zoom meeting" entry (file+headline "~/Sync/Org/Todo.org" "Meetings")
         "* TODO Zoom, %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n"
         :empty-lines 1)) )

;; Agenda is constructed from org files in ONE directory
(setq org-agenda-files '("~/Sync/Org"))

;; refile to targets defined by the org-agenda-files list above
(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;; store DONE time in the drawer
(setq org-log-done (quote time))
(setq org-log-into-drawer t)

;; Ask and store note if rescheduling
(setq org-log-reschedule (quote note))

;; syntax highlight latex in org files
(setq org-highlight-latex-and-related '(latex script entities))

;; define the number of days to show in the agenda
(setq org-agenda-span 14
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")

;; default duration of events
(setq org-agenda-default-appointment-duration 60)
(setq org-agenda-prefix-format '(
  ;;;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
                                 (agenda  . "  •  %-12:c%?-12t% s")
                                 (timeline  . "  % s")
                                 (todo  . " %i %-12:c")
                                 (tags  . " %i %-12:c")
                                 (search . " %i %-12:c")))

(use-package gnuplot
  :init
  (message "Use-package: gnuplot for babel installed"))

;; languages I work in via babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t) (emacs-lisp . t) (shell . t) (python . t)))
;; stop it asking if I'm sure about evaluation
(setq org-confirm-babel-evaluate nil)

;; org-mode
(add-hook 'org-mode-hook 'hl-line-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(add-hook 'latex-mode-hook 'hl-line-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)

;; setup files ending in “.m4” to open in LaTeX-mode
;; for use in lecture note construction
(add-to-list 'auto-mode-alist '("\\.m4\\'" . latex-mode))
