;;;;
;;;; Some content from https://github.com/daedreth/UncleDavesEmacs/blob/master/config.org
;;;;

;;;;----------------------------------------
;;;; LOOK
;;;;----------------------------------------

;; Turn off the menu
(menu-bar-mode -1)

;; Check if any new frame is in a graphical environment
;; Doing this on a per-frame basis allows it to deal with
;; daemon mode, since otherwise the daemon always starts
;; w/o a GUI.
(defun new-frame-setup (frame)
  (if (display-graphic-p frame)
      (progn (message "Window system")
	     (load-theme 'doom-dracula t)
	     (scroll-bar-mode -1)
	     (tool-bar-mode -1)
	     ;; below are already defined via Alacritty for terminal mode
	     (global-set-key (kbd "M-l") 'forward-char) 
	     (global-set-key (kbd "M-i") 'previous-line) 
	     (global-set-key (kbd "M-j") 'backward-char) 
	     (global-set-key (kbd "M-k") 'next-line)
	     )
    (progn(message "Not a window system")
	  (load-theme 'doom-gruvbox t) )
    )
  )

;; Run for already-existing frames
;; (mapc 'new-frame-setup (frame-list))
;; Run when a new frame is created
(add-hook 'after-make-frame-functions 'new-frame-setup)

;; this stops the cursor recentering on leaving the page
;; ie. stop scrolling by 0.5 page
(setq scroll-conservatively 101 )

;; use YASnippet instead of abbrev
(setq-default abbrev-mode nil)

;;; which-key pop up of incomplete command sequence completions
(use-package which-key
  :ensure t
  ; don't show whick-key-mode in modeline
  :delight which-key-mode 
  :config
  (which-key-mode)
  )

;;;; STARTUP dashboard
(use-package dashboard
    :ensure t
    :diminish dashboard-mode
    :config
    (setq dashboard-banner-logo-title "Quickstart!")
    (setq dashboard-startup-banner "/home/hewitt/CURRENT/dot.local/share/icons/hicolor/128x128/apps/emacs.png")
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 5)
			    (agenda . 4)))
    (dashboard-setup-startup-hook)
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    )

;;;; DELIGHT: this allows relabelling or removing what appears in the
;;;; modeline
(use-package delight
  :ensure t
  :no-require t
  :init (message "Use-package: delight")
  )
(delight 'eldoc-mode "Eld." 'eldoc)
(delight 'undo-tree-mode "Ut." 'undo-tree)
(delight 'abbrev-mode "Ab." 'abbrev)
(delight 'helm-mode "Helm." 'helm)

;;;;
;;;; CURRENT theme select
;;;;
;; Disable all previously selected themes
(mapcar #'disable-theme custom-enabled-themes)

(use-package doom-themes
  :ensure t
  )

(if (display-graphic-p)
    ( progn (message "Window system determined.")
	    (load-theme 'doom-dracula t)
	    (scroll-bar-mode -1)
	    (tool-bar-mode -1)
	    ;; below are already defined via Alacritty for terminal mode
	    (global-set-key (kbd "M-l") 'forward-char) 
	    (global-set-key (kbd "M-i") 'previous-line) 
	    (global-set-key (kbd "M-j") 'backward-char) 
	    (global-set-key (kbd "M-k") 'next-line) )
  ( progn (message "Terminal system determined.") ; else
	  (load-theme 'doom-gruvbox t) )
  )



;;;;
;;;; MODE LINE that's better than default but not too heavy like spacemacs
;;;;
(use-package doom-modeline
  :ensure t
  :init (message "Use-package: doom-modeline")
  :hook (after-init . doom-modeline-mode)
  :config
  ;; Whether display icons or not (if nil nothing will be showed).
  (setq doom-modeline-icon t)
  ;; Display the icon for the major mode. 
  (setq doom-modeline-major-mode-icon t )
  ;; Display color icons for `major-mode' 
  (setq doom-modeline-major-mode-color-icon t)
  ;; Display minor modes or not?
  (setq doom-modeline-minor-modes t)
  ;; Whether display icons for buffer states.
  (setq doom-modeline-buffer-state-icon t)
  ;; Whether display buffer modification icon.
  (setq doom-modeline-buffer-modification-icon t)
  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)
  ;; If non-nil, only display one number for checker information if applicable.
  ;(setq doom-modeline-checker-simple-format t)
  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 8)
  ;; Whether display perspective name or not. Non-nil to display in mode-line.
  (setq doom-modeline-persp-name t)
  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  ;(setq doom-modeline-lsp t)
  )


;;;;
;;;; COLOURISE delimiters in programming or latex
;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (message "Use-package: rainbow delimiters")
  :config
  (rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'latex-mode-hook 'rainbow-delimiters-mode)
  )

;;;;
;;;; Highlight the current mode
;;;;
(global-hl-line-mode 1)
(set-face-background hl-line-face "grey20")


;;;;-------------------------------------
;;;; INTERACTION
;;;;-------------------------------------

;;;;
;;;; sort out cut and paste in Wayland environment
;;;;
(setq x-select-enable-clipboard t)
(defun txt-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "wl-copy" ))
  ;(call-process-region (point-min) (point-max) "wl-copy" nil 0 nil ))
  )
;; (defun txt-paste-function()
;;   (let ((xsel-output (shell-command-to-string "wl-paste")))
;;     (unless (string= (car kill-ring) xsel-output)
;;       xsel-output ))
;;   )
(setq interprogram-cut-function 'txt-cut-function)
;; (setq interprogram-paste-function 'txt-paste-function)

(use-package key-chord
  :ensure t
  :init
  (progn
    ;; Max time delay between two key presses to be considered a key chord
    (setq key-chord-two-keys-delay 0.1) ; default 0.1
    ;; Max time delay between two presses of the same key to be considered a key chord.
    ;; Should normally be a little longer than `key-chord-two-keys-delay'.
    (setq key-chord-one-key-delay 0.2) ; default 0.2    
    (key-chord-mode 1)
    ;; k can be bound too
    ;(key-chord-define-global "uu"     'undo-tree-undo)
    (key-chord-define-global "kk"     'kill-whole-line)
    (key-chord-define-global "jj"     'avy-goto-word-1)
    (key-chord-define-global "jl"     'avy-goto-line)
    (key-chord-define-global "qq"     'helm-buffers-list)
    (key-chord-define-global "qb"     'bookmark-set)
    (key-chord-define-global "qj"     'bookmark-jump)
    (key-chord-define-global "qo"     'other-window)
    ;(key-chord-define-global "hh"     'previous-buffer)
    ;(key-chord-define-global "HH"     'next-buffer)
    )
  )

;;;; AVY
(use-package avy
  :ensure t
  :defer t
  :init
  (message "Use-package: avy")
  :config
  (setq avy-background t)
  )

;;;; Better undo
(use-package undo-tree
  :ensure t
  :defer t
  :init
  (message "Use-package: undo-tree")
  (global-undo-tree-mode)
  )


;;;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
;;;; This COMMAND will load a buffer if it changes on disk, which is
;;;; super handy if editing from multiple machines over long periods.
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)
    )
(global-auto-revert-mode 1)

;;;; location of my snippets -- has to go before yas-reload-all
(setq-default yas-snippet-dirs '("/home/hewitt/.emacs.d/my_snippets"))
;;;; include yansippet and snippets
(use-package yasnippet
  :delight (yas-minor-mode " YaS.")
  :ensure t
  :defer t
  :init
  (message "Use-package: yasnippet")
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; hooks for YASnippet in Latex and C++;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'c++-mode-hook 'yas-minor-mode)
  (add-hook 'latex-mode-hook 'yas-minor-mode)
  ;;;; remove default keybinding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;;;; redefine my own key
  (define-key yas-minor-mode-map (kbd "M-]") yas-maybe-expand)
  ;;;; remove default keys for navigation
  (define-key yas-keymap [(tab)]       nil)
  (define-key yas-keymap (kbd "TAB")   nil)
  (define-key yas-keymap [(shift tab)] nil)
  (define-key yas-keymap [backtab]     nil)
  ;;;; redefine my own keys
  (define-key yas-keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "M-p") 'yas-prev-field)  
  (yas-reload-all)
  )


;;;; move focus when splitting a window
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

;;;; move focus when splitting a window
(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)


;;;; editorconfig allows specification of tab/space/indent
(use-package editorconfig
  :ensure t
  :defer t
  :delight (editorconfig-mode "EC.")
  :init
  (message "Use-package: editorconfig")
  :config
  (editorconfig-mode 1))

;;;;
;;;; HELM MODE
;;;;
;; taken from Sacha Chua's config
(use-package helm
  :ensure t
  :delight ; dont show in the modeline
  :defer t
  :init
  (message "Use-package: helm")
  (progn ; evaluate each of these in sequence
    (require 'helm-config)
    (setq helm-candidate-number-limit 12)
    ;; ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay                       0.0 
          ;helm-input-idle-delay                 0.01                                        
          ;helm-quick-update                     t
          ;helm-M-x-requires-pattern             nil
          helm-ff-skip-boring-files             t
     	  ;helm-split-window-in-side-p           t 
     	  helm-move-to-line-cycle-in-source     t
     	  ;helm-ff-search-library-in-sexp        t 
     	  helm-scroll-amount                    2
     	  helm-ff-file-name-history-use-recentf t
     	  helm-echo-input-in-header-line        t
	  helm-buffers-fuzzy-matching           t
	  helm-recentf-fuzzy-match              t
	  helm-M-x-fuzzy-match                  t
          helm-autoresize-max-height            30 ;; fixed height 
	  helm-autoresize-min-height            30)
    (helm-mode))
  :bind
  (("C-c h" . helm-command-prefix)
   ("C-h a" . helm-apropos)
   ("C-x C-b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)   ; replace standard Meta-x
   ("C-s" . helm-occur) ; replace standard I-search
   )
  :config
  )

