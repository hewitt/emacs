;; -*- lexical-binding: t; -*-; ad

(setq-default untrusted-content t)

(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities ; prefer ELPA to MELPA
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5 )))
(package-initialize)

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; add a location for supporting elisp code
(add-to-list 'load-path "~/.emacs.d/elisp")

;; skip auto backups
(setq make-backup-files nil)
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
;;
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
;; left to right only
(setq-default bidi-paragraph-direction 'left-to-right) ; buffer-local
;; max no of bytes to read from subprocess in a single chunk
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

(global-auto-revert-mode)

;; use both line & column numbers
(setq rh/modeline-position (list " L%l C%c"))

;; this gets hooked later to update modeline colours when the theme is changed
(defun rh/modeline-update ()
  "Update style of the modeline faces to match the choice of ef-theme."
  (ef-themes-with-colors
    (custom-set-faces
     `(mode-line ((,c :background ,bg-mode-line 
                      :foreground ,fg-main :box (:line-width (1 . 6) :color ,bg-mode-line))))
     `(mode-line-inactive ((,c :background ,bg-alt :box (:line-width (1 . 1) :color ,fg-dim)))))
    ))

;; taken from Prot
(defvar-local prot-modeline-narrow
  '(:eval
    (when (and (mode-line-window-selected-p)
               (buffer-narrowed-p)
               (not (memq major-mode '(Info-mode help-mode message-mode special-mode))))
      (propertize " Narrow " 'face 'prot-modeline-indicator-cyan-bg)))
"Mode line construct to report the narrowed state of the current buffer.")
(put 'prot-modeline-narrow 'risky-local-variable t)


;; if file-truename is "~/a/b/../c/d/filename" then
;; show "a/b/../c/d" in a less obvious face
;; only if the buffer is selected
(defvar-local rh/modeline-filepath-if-selected
  ; not all buffers have a filepath (e.g. terminal/scratch)
  '(:eval (if buffer-file-name  
	      (when (mode-line-window-selected-p) 
		(propertize 
		 (string-join (seq-subseq (split-string buffer-file-truename "/") 1 -1) "/") 
		 'face 'shadow)                                      
		) 
	    )
	  )
  "Modeline construct for a path to current filename with a shadow face when window is selected."
  )
;; This is required, see C-h v mode-line-format
(put 'rh/modeline-filepath-if-selected 'risky-local-variable t)

;; filename in a more obvious (warning) colour
(defvar-local rh/modeline-filename
  '(:eval (if buffer-file-name  ; not all buffers have a filename
	     (propertize 
	      (string-join (seq-subseq (split-string buffer-file-truename "/") -1 nil)) 
	      'face 'error)
	   )
	 )
  "Modeline construct for just the filename with an error face."
  )
;; This is required, see C-h v mode-line-format
(put 'rh/modeline-filename 'risky-local-variable t)


(defvar-local rh/modeline-major-mode
'(:eval (when (mode-line-window-selected-p) 
	  (propertize (nth 0
			   (split-string
			    (capitalize (symbol-name major-mode)) "-Mode")
			   )
		      'face 'success)
	  )
	)
"Modeline construct for major mode with 'mode' removed."
)
;; This is required, see C-h v mode-line-format
(put 'rh/modeline-major-mode 'risky-local-variable t)


(defvar-local rh/modeline-separator
  '(:eval (propertize " | " 'face 'shadow) ) 
  "Modeline separator symbol."
  )
;; This is required, see C-h v mode-line-format
(put 'rh/modeline-separator 'risky-local-variable t)

(setq-default rh/modeline-format
	      '(
                "%e"
		mode-line-front-space
                ;; e.g. fire symbol below for unsaved buffer is
                ;; selected via (C-x 8 RET)
                (:eval (if (buffer-modified-p)
                           (propertize "🔥 " 'face 'error)
                         (propertize "- " 'face 'shadow)
                         )
                       )
		prot-modeline-narrow
		rh/modeline-filepath-if-selected
		"/"
		rh/modeline-filename
                mode-line-format-right-align
		rh/modeline-separator
		rh/modeline-major-mode
		" "
                (vc-mode vc-mode)
		rh/modeline-separator
                rh/modeline-position 
		"  "
                )
              )

;; make the above definition the mode-line
(setq-default mode-line-format rh/modeline-format)
;; apply the hook to keep modeline colours up to date with current theme
(add-hook 'ef-themes-post-load-hook #'rh/modeline-update)

(use-package ef-themes
  :init
  ;(ef-themes-take-over-modus-themes-mode 1)
  :config
  ;; All customisations here.
  ;(setq modus-themes-mixed-fonts t)
  ;(setq modus-themes-italic-constructs t)
  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-select 'ef-eagle)
)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(use-package indent-bars
  :hook
  ((python-ts-mode yaml-ts-mode latex-mode org-mode ) . indent-bars-mode))

;; variable is buffer local
(setq-default fill-column 79) 
(global-display-fill-column-indicator-mode)

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

(use-package fontaine
  :defer t)

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
        (aporetic
         :default-family "Aporetic Serif Mono"
         :default-weight normal
         :default-height 125
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Aporetic Serif"
         :variable-pitch-weight normal
         :variable-pitch-height 120
	 :mode-line-family "Aporetic Serif"
	 :mode-line-weight normal
	 :mode-line-active-height 110
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil ; use whatever the underlying face has
         :italic-slant italic
         :line-spacing 1)
        (aporeticHDPI
         :default-family "Aporetic Serif Mono"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Aporetic Serif"
         :variable-pitch-weight normal
         :variable-pitch-height 140
	 :mode-line-family "Aporetic Serif"
	 :mode-line-weight normal
	 :mode-line-active-height 140
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil ; use whatever the underlying face has
         :italic-slant italic
         :line-spacing 1)
	(terminus
         :default-family "Terminus"
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
         :line-spacing 1)))

(defun new-frame-setup (frame)
  (if (display-graphic-p frame)
      (progn
	(fontaine-mode 1)
	(define-key global-map (kbd "C-c f") #'fontaine-set-preset)
	(fontaine-set-preset (or (fontaine-restore-latest-preset) 'aporetic))
	)
    (message "no GUI => no fontaine")))

;; Run for already-existing frames
(mapc 'new-frame-setup (frame-list))
;; Run when a new frame is created
(add-hook 'after-make-frame-functions 'new-frame-setup)

;; Persist the latest font preset when closing/starting Emacs and
;; while switching between themes.
; (fontaine-mode 1)

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

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(use-package emacs
  :ensure nil
  :demand t
  :init
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

  The generic `keyboard-quit' does not do the expected thing when
  the minibuffer is open.  Whereas we want it to close the
  minibuffer, even without explicitly focusing it.

  The DWIM behaviour of this command is as follows:

  - When the region is active, disable it.
  - When a minibuffer is open, but not focused, close the minibuffer.
  - When the Completions buffer is selected, close it.
  - In every other case use the regular `keyboard-quit'."
      (interactive)
      (cond
       ((region-active-p)
        (keyboard-quit))
       ((derived-mode-p 'completion-list-mode)
        (delete-completion-window))
       ((> (minibuffer-depth) 0)
        (abort-recursive-edit))
       (t
        (keyboard-quit))))
    :bind
    ("C-g" . prot/keyboard-quit-dwim)
    :config
    ;; I have never seen a user say "no" to loading a theme they have
    ;; downloaded.  Technically, any Elisp file can run arbitrary code,
    ;; so this is not doing much on the security front.
    (setq custom-safe-themes t)
    (setq use-short-answers t)
    (setq read-answer-short t)
    (setq help-window-select t) ; also check `display-buffer-alist' below
    (setq help-window-keep-selected t) ; Emacs 29
    (setq find-library-include-other-files nil) ; Emacs 29
    (setq window-combination-resize t)
    (setq save-interprogram-paste-before-kill t)
    ;; Do not jump to the current line in `*occur*' buffers.  The reason
    ;; is that you are already on that line: you want to do `occur' to
    ;; get more than that (and, presumably, to do something with the
    ;; results such as to edit them with `occur-edit-mode').
    (setq list-matching-lines-jump-to-current-line nil)
    (setq completion-category-defaults nil))

;;;; Dired
(use-package dired
  :ensure nil
  :config
  ;; Most people I have talked to prefer a single Dired buffer.
  ;; Personally I like the many Dired buffers, but I understand why
  ;; this feels overwhelming.
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29
  (setq wdired-create-parent-directories t))

;;;; Diff
(use-package diff
  :config
  ;; You cannot expect the syntax highlighting of themes to look
  ;; equally readabable against what typically are red and green
  ;; backgrounds.  This should be opt-in by default, not opt-out.
  (setq diff-font-lock-syntax nil))

;;;; Ediff
(use-package ediff
  :config
  ;; Ediff is virtually unusable without those.  Especially on tiling
  ;; window managers.  But even on a regular desktop environment it is
  ;; confusing and cumbersome to have the control panel in another
  ;; frame.
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;;; SHR
(use-package shr
  :config
  ;; t is bad for accessibility and generally awkward for HTML email
  ;; (especially with dark themes).
  (setq shr-use-colors nil)
  ;; This option should not exist, given `variable-pitch-mode'.
  ;; Furthermore, its default value runs counter to almost everything
  ;; else in Emacs which just uses the `default' face.
  (setq shr-use-fonts nil))

(setq lpr-command "gtklp")
(setq ps-lpr-command "gtklp")

(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical t)                    
(setq switch-to-buffer-in-dedicated-window 'pop)  
(setq split-height-threshold 80)
(setq split-width-threshold 120)
(setq window-min-height 5)
(setq window-min-width 90)

;;(setq display-buffer-alist 'nil) ; to remove all preferences
(setq display-buffer-alist
      `(
        ("\\*Org Agenda\\*" ; 
         (display-buffer-full-frame)
         )	
        ((derived-mode . mu4e-compose-mode)
         (display-buffer-in-side-window)
         (dedicated . t) ; don't reuse this buffer for other things
         (window-width . 90)
         (side . right) ; put it on the right side
         )	
        ((or (derived-mode . mu4e-headers-mode)
             (derived-mode . mu4e-main-mode )) 
         (display-buffer-reuse-mode-window) ; don't always open a new window
         (dedicated . t) ; don't reuse this buffer for other things
         )
        ((derived-mode . pdf-view-mode) ; puts the PDF into a new frame
	 (display-buffer-pop-up-frame)
	 (reusable-frames . visible)
	 )
        ("\\*Org Select\\*"  ; org-capture template selection
	 (display-buffer-full-frame)
	 )
	("\\(\\*Capture\\*\\|CAPTURE-.*\\)"; match all the usual capture buffers
	 (display-buffer-full-frame) 
	 )
        ;; ("\\*Org \\(Select\\|Note\\)\\*"  ; put other Org stuff at the bottom
        ;;  (display-buffer-in-side-window)
        ;;  (dedicated . t)   ; don't reuse this buffer for other things
        ;;  (side . bottom)
        ;;  ;;(window-parameters . ((mode-line-format . none)))  ; turn off the mode line
        ;;  )          
        ))

;; move focus when splitting a window
(defun my/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

;; move focus when splitting a window
(defun my/split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'my/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'my/split-and-follow-vertically)
;; short-cut the other-window key
(keymap-set global-map "M-o" 'other-window)
;; smaller divider between split windows
(setq window-divider-default-right-width 2)
(set-face-foreground 'vertical-border "#aaaaaa")  ;; color of the divider in text mode
(custom-set-faces '(window-divider ((t (:foreground "#aaaaaa"))))) ;; color of the divider in graphical mode
(window-divider-mode)

(use-package avy
  :init
  (message "Use-package: avy")
  :config
  ;; this forces this key bind not to be overwritten by (eg) org/latex-modes
  (bind-keys* ("C-;" . avy-goto-char-2) ))

(use-package expreg
  :init
  (message "Use-package: expreg")
  :config
  (bind-keys*
   ("C-." . expreg-expand)
   ("C-," . expreg-contract)))

;; short-cut to edit the init.el configuration file
(defun rh/config-visit ()
  (interactive)
  (find-file "~/CURRENT/NixConfig/outOfStore/.emacs.d/config.org") )

;; short-cut to edit the init.el configuration file
(defun rh/todo-visit ()
  (interactive)
  (find-file "~/Sync/Org/Todo.org") )

;; put magit in a new tab
(defun rh/magit-status-new-tab ()
  "Open magit-status in a new tab and make it fill the tab."
  (interactive)
  (tab-bar-new-tab)
  (call-interactively #'magit-status)
  (delete-other-windows) )

(defvar-keymap rh-prefix-org-map
  :doc "Prefix map for Org mode."
  "c" #'org-capture
  "a" #'org-agenda
  "j" #'org-journal-new-entry
  "t" #'org-babel-tangle)

(defvar-keymap rh-prefix-display-map
  :doc "Prefix map for display features."
  "+" #'global-text-scale-adjust
  "f" #'fontaine-set-preset)

(defvar-keymap rh-prefix-tab-map
  :doc "Prefix map for display features."
  "t" #'tab-switcher
  "d" #'tab-close
  "n" #'tab-new)

;; Define a key map with commands and (potentially nested) key maps
(defvar-keymap my-prefix-map
  :doc "My prefix key map."
  "o" rh-prefix-org-map
  "d" rh-prefix-display-map
  "t" rh-prefix-tab-map
  "s" #'consult-notes-search-in-all-notes
  "T" #'rh/todo-visit
  "e" #'rh/config-visit
  "m" #'mu4e
  "f" #'dired
  "b" #'consult-buffer
  )

;; Define how the nested keymaps are labelled in `which-key-mode'.
(which-key-add-keymap-based-replacements my-prefix-map
  "o" `("Org" . ,rh-prefix-org-map)
  "d" `("display" . ,rh-prefix-display-map)
  "t" `("tabs" . ,rh-prefix-tab-map)
  )

;; Bind the prefix key map to a key.  Notice the absence of a quote for
;; the map's symbol.
(keymap-set global-map "C-z" my-prefix-map)

;; editorconfig allows local specification of tab/space/indent
;; using a config file in the directory
(use-package editorconfig
  :init
  (message "Use-package: EditorConfig")
  :config
  (editorconfig-mode 1) )

(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))

(define-abbrev text-mode-abbrev-table "rhzoom" "https://zoom.us/my/rich.hewitt")
(define-abbrev text-mode-abbrev-table "rhoffice" "Alan Turing Rm 2.227")
(define-abbrev global-abbrev-table "rhdate" ""
  (lambda () (insert (format-time-string "[%Y-%m-%d %a %H:%M]"))))

(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'org-mode-hook 'abbrev-mode)

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

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode) 
  :config
  (require 'smartparens-config))

(use-package transpose-frame)

(use-package diff-hl    
  :init
  (message "Use-package: diff-hl")
  :hook
  (dired-mode . diff-hl-dired-mode)
  (prog-mode . diff-hl-show-hunk-mouse-mode))

(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1) 
(unless (display-graphic-p)
  (diff-hl-margin-mode 1))

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
  (add-hook 'latex-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'org-mode-hook 'eglot-ensure)
  (add-hook 'text-mode-hook 'eglot-ensure)
  :config
  ; config entries run after package loading has happened
  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("ccls")))
  (add-to-list 'eglot-server-programs '(latex-mode . ("digestif")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  ;;(add-to-list 'eglot-server-programs '(text-mode . ("harper-ls" "--stdio")))
  ;;(add-to-list 'eglot-server-programs '(org-mode . ("harper-ls" "--stdio")))    
  ;; (add-to-list
  ;;  'eglot-server-programs
  ;;  '((latex-mode)
  ;;    . ("rass"
  ;; 	"--"
  ;; 	"harper-ls" "--stdio"
  ;; 	"--"
  ;; 	"digestif"
  ;; 	)))
  )

;; (setq-default eglot-workspace-configuration
;; 	      '(:harper-ls (:userDictPath ""
;; 			    :workspaceDictPath ""
;; 			    :fileDictPath ""
;; 			    :linters (:SpellCheck t
;; 				      :UnclosedQuotes t
;; 				      :WrongQuotes t
;; 				      :LongSentences t
;; 				      :RepeatedWords t
;; 				      :Spaces t
;; 				      :Matcher t
;; 				      :CorrectNumberSuffix t)
;; 			    :diagnosticSeverity "hint"
;; 			    :dialect "British"
;; 			    :maxFileLength 120000
;; 			    :excludePatterns [])))

(use-package direnv
  :config
  (direnv-mode))

(use-package pyvenv-auto
  :hook ((python-ts-mode . pyvenv-auto-run)))

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

;;[2026-06-26 Fri 17:12] No longer required for emacs 31+
;;(use-package corfu-terminal)
;;(unless (display-graphic-p)
;;  (corfu-terminal-mode +1))

(use-package corfu-prescient
  :init
  (message "Use-package: corfu-prescient"))

;; NIX language mode
(use-package nix-mode
  :mode "\\.nix\\'" )

;; my default gnuplot extension
(add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))
;; Octave/Matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; Nix language
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; MAGIT
(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status)
  :init
  (message "Use-package: Magit installed"))

(defun rh/org-capture-after-finalize-clean-up ()
  "If add this to 'org-capture-after-finalize-hook' then closing the
'org-capture' session will also delete the frame. This is useful for
captures initiated from the window manager (WM) level rather than within
emacs itslef. Before the frame is deleted, we remove the hook, so that
similar captures later initiated from within emacs don't continue to delete
the frame!

Initiate the capture from the WM via something like:
emacsclient -cn -e '(progn (add-hook (quote org-capture-after-finalize-hook) (quote rh/org-capture-after-finalize-clean-up))(org-capture nil \"t\"))' -F '((name . \"**Capture**\"))'

Note that you can use the **Capture** title to have the pop-up frame treated differently, eg. floating rather than tiled. Also the use of (quote ...) may make this simple when processed as a WM shell command. 
"
  (remove-hook 'org-capture-after-finalize-hook 'rh/org-capture-after-finalize-clean-up)    
  (delete-frame nil t))

(defun rh/org-agenda-quit-clean-up ()
  "Adding this to 'org-agenda-quit' then closing the
'org-agenda-list' session will also delete the frame. This is useful for
captures initiated from the window manager (WM) level rather than within
emacs itslef. Before the frame is deleted, we remove the advice, so that
viewing the org-agenda-list later initiated from within emacs doesn't
continue to delete the frame!

Initiate the capture from the WM via something like:
emacsclient -cn -e '(progn (add-hook (quote org-agenda-quit) (quote rh/org-agenda-quit-clean-up))(org-agenda-list))' -F '((name . \"**Agenda**\"))'

Note that you can use the **Agenda** title to have the pop-up frame treated differently, eg. floating rather than tiled. Also the use of (quote ...) may make this simple when processed as a WM shell command. 
"
  (interactive)
  (advice-remove 'org-agenda-quit 'rh/org-agenda-quit-clean-up)
  ;;(advice-remove 'org-agenda-Quit 'rh/org-agenda-quit-clean-up)    
  (delete-frame nil t))

(use-package org
  :init
  (message "Use-package: Org") )

(use-package org-modern
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags t
   org-tags-column -77 ; right-align to 77th column
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers nil ; show the *...* for bold
   org-pretty-entities nil ; stop underscore subscripting 

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis " … ")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  ;; symbols
  (set-face-attribute 'org-modern-symbol nil :family "Aporetic Serif Mono")
  )

;; some appearance tweaks:
;;
;; replace emphasis with colors in Org files
(setq org-emphasis-alist
      '(("*" rh/org-emphasis-bold)
        ("/" rh/org-emphasis-italic)
        ("_" rh/org-emphasis-underline)
        ("=" org-verbatim verbatim)
        ("~" org-code verbatim)
        ("+" (:strike-through t))))
;;
;; colorise text for emphasis
(defface rh/org-emphasis-bold
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff8059"))
  "My bold emphasis for Org.")
;;
(defface rh/org-emphasis-italic
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#005e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#44bc44"))
  "My italic emphasis for Org.")
;;
(defface rh/org-emphasis-underline
  '((default :inherit underline)
    (((class color) (min-colors 88) (background light))
     :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#d0bc00"))
  "My underline emphasis for Org.")

;; custom capture
(require 'org-capture)
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
(setq org-agenda-span 90
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

(require 'denote)

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/CURRENT/PNL/Denote_env/"))
(setq denote-known-keywords '("research" "admin" "industry" "teaching" "home" "attachment"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(title keywords))

;; We allow multi-word keywords by default.  The author's personal
;; preference is for single-word keywords for a more rigid workflow.
(setq denote-allow-multi-word-keywords t)

(setq denote-date-format nil) ; read doc string

;; By default, we fontify backlinks in their bespoke buffer.
(setq denote-link-fontify-backlinks t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;;(require 'denote-dired)
(setq denote-dired-rename-expert nil)

(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(with-eval-after-load 'org-capture    
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; I still like "org-journal" rather than using "denote".
(use-package org-journal
  :init
  (message "Use-package: Org-journal")
  :config
  (setq org-journal-dir "~/CURRENT/PNL/JNL/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y_%m_%d"
        org-journal-time-prefix "  - "
        org-journal-time-format nil
        org-journal-file-type 'monthly))

;; org-mode
(add-hook 'org-mode-hook 'hl-line-mode)
;;(add-hook 'org-mode-hook 'flyspell-mode) ; move to harper-ls
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
;; org-modern
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(setq ispell-program-name "ispell")
(setq ispell-dictionary "english")
(setq ispell-personal-dictionary "/home/hewitt/.emacs.d/my_dictionary")

(add-hook 'latex-mode-hook 'hl-line-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)

;; setup files ending in “.m4” to open in LaTeX-mode
;; for use in lecture note construction
(add-to-list 'auto-mode-alist '("\\.m4\\'" . latex-mode))

;; pdf tools for organising and annotating PDF
(use-package pdf-tools
  :config
  (pdf-tools-install))

;; defines mu4e exists, but holds off until needed
;;(autoload 'mu4e "mu4e" "Launch mu4e and show the main window" t)
(require 'mu4e)

;; how to get mail
(setq mu4e-get-mail-command "mbsync Work"
      mu4e-maildir (expand-file-name "~/CURRENT/mbsyncmail")
      mu4e-mu-binary (executable-find "mu"))
;; DONT auto GET 
(setq mu4e-update-interval nil)

;; I don't sync Deleted Items & largely do permanent
;;  delete via "D" rather than move to trash via "d" 
(setq mu4e-trash-folder  "/Trash") 
;; [2018] : this stops errors associated with duplicated UIDs -- LEAVE IT HERE!
(setq mu4e-change-filenames-when-moving t)
;; show thread but don't bring back related emails that have been moved
(setq mu4e-headers-show-thread t
      mu4e-headers-include-related nil
      mu4e-headers-visible-lines 20
      mu4e-headers-results-limit 200)
;; rich text emails are converted using 'shr'
;; they are displayed using 'shr-face'
;; and for a dark background the 'mu4e' manual suggests:
(setq shr-color-visible-luminance-min 80)

;; Define what headers to show
;; in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; best to only use nil for the last field.
(setq mu4e-headers-fields
      '((:human-date          .  10)   ;; alternatively, use :date
        (:flags               .   5)
        (:recipnum            .   3)
        (:from-or-to          .  30)
        (:thread-subject      . nil))  ;; alternatively, use :thread-subject
      )
;; shortcut keys are used in the main-view
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"          . ?i)
         ("/Sent"           . ?s)
         ("/Trash"          . ?t)
         ("/Drafts"         . ?d)
         ("/BULK"           . ?b)))
;; bookmarks
(setq mu4e-bookmarks
      ' ((:name "Unread" :query "flag:unread AND NOT flag:trashed AND NOT maildir:/JUNK" :key 117) ; bu
         (:name "Today" :query "date:today..now" :key 116)                   ; bt
         (:name "Week" :query "date:7d..now" :hide-unread t :key 119)        ; bw
         (:name "Attachment" :query "flag:a" :key 97)                        ; ba
         (:name "Flagged"    :query "flag:F" :key 102)                       ; bf
         ))       
;; don't auto update in the headers view, wait for return to main view
(setq mu4e-headers-auto-update nil)

;; configure for msmtp as this is easy to test from the CLI
(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
;; Note: sent mails should appear in O365 sent list
;; O365 uses "Sent Items" in the web interface but this
;; appears as just "Sent" with mbsync set to "Patterns *"
(setq mu4e-sent-folder   "/Sent")
;; sent messages are copied into the 'mu4e-sent-folder' defined above
;; Make sure that .davmail.properties has .smtpSaveInSent=false otherwise we get
;; 2 copies in the O365 "Sent Items" folder
(setq mu4e-sent-messages-behavior 'sent)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "richard.hewitt@manchester.ac.uk"
      user-mail-address "richard.hewitt@manchester.ac.uk"
      user-full-name  "Rich Hewitt")
;; compose signature
(setq message-signature-file "~/CURRENT/dot.signature")
(setq mu4e-compose-signature-auto-include t)
;; define where to put draft email
(setq mu4e-drafts-folder "/Drafts")
;; spell check during compose
(add-hook 'mu4e-compose-mode-hook
          (defun rh/do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 80)
            (flyspell-mode)
            ;; turn off autosave, otherwise we end up with multiple
            ;; versions of sent/draft mail being sync'd
            (auto-save-mode -1)))
;; Couple to Org -- not sure if this is strictly required or not?
;(require 'mu4e-org)

(use-package age
  :demand
  :custom
  (age-program "rage")   ; 'rage' is the rust implementation of 'age' that supports pinentry
  (age-default-identity "~/CURRENT/AGE/yubikey-bb978fd1-identity.txt")
  (age-default-recipient
   '("~/CURRENT/AGE/recovery-recipient.pub"            ; cold-storage recovery
     "~/CURRENT/AGE/yubikey-bb978fd1-recipient.pub"))  ; active hardware key
  :config
  (setq age-armor nil) ;; don't convert to ASCII so I can see multiple key headers from the CLI
  (age-file-enable))
