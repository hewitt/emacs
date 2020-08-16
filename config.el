;;;;
;;;; CONFIGURE MELPA AND GNU ARCHIVES
;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;;;
;;;; CONFIGURE USE-PACKAGE TO AUTOLOAD THINGS : https://github.com/jwiegley/use-package
;;;;
(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package)
)

;; this is the standard use-package invocation if it is in ~/.emacs.d
(eval-when-compile
 (require 'use-package)
)

;; Keep custom settings in a separate file to not pollute this one
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(defun system-is-Blasius ()
  (interactive)
  "True for Blasius laptop"
  (string-equal system-name "Blasius"))
(defun system-is-Orthanc ()
  (interactive)
  "True for Orthanc desktop"
  (string-equal system-name "Orthanc"))

;; move backups to stop *~ proliferation
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; have mouse input in the terminal
;; the disadvantage is you need to SHIFT+middle mouse to paste in the terminal
(xterm-mouse-mode 1)
;; Turn off the menu
(menu-bar-mode -1)
;; this stops the cursor recentering on leaving the page
;; ie. stop scrolling by 0.5 page
(setq scroll-conservatively 101 )
;; replace annoying yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; don't end sentences with a single space
(setq sentence-end-double-space nil)
;; From Prelude: reduce the frequency of garbage collection by making it happen on
;; each 5MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 5000000)
;; warn when opening files bigger than 50MB
(setq large-file-warning-threshold 50000000)
;; set this to avoid having to reply y/n every time you open a symbolic link in a git repo
(setq vc-follow-symlinks nil)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
;; This COMMAND will load a buffer if it changes on disk, which is
;; super handy if editing from multiple machines over long periods.
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)
    )
(global-auto-revert-mode 1)

(use-package delight
  :ensure t
  ;:no-require t
  :init (message "Use-package: Delight")
  )
(delight 'eldoc-mode "Eld." 'eldoc)
(delight 'undo-tree-mode "Ut." 'undo-tree)
(delight 'abbrev-mode "Ab." 'abbrev)
;(delight 'helm-mode "Helm." 'helm)

;; dashboard runs at startup by default
(use-package dashboard
    :ensure t
    :delight dashboard-mode
    :init
    (message "Use-package: Dashboard")
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

(mapcar #'disable-theme custom-enabled-themes)
(use-package doom-themes
  :ensure t
  )
;; choose theme based on text terminal vs GUI
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
;; Reapply the theme on every new frame
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
	     ;; fall back cursor keys
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

;; modeline
(use-package doom-modeline
  :ensure t
  :init (message "Use-package: Doom-modeline")
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

;; colourise those brackets
(use-package rainbow-delimiters
  :ensure t
  :init
  (message "Use-package: Rainbow delimiters")
  :config
  (rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'latex-mode-hook 'rainbow-delimiters-mode)
  )

;; cut and paste in Wayland environmen
(setq x-select-enable-clipboard t)
(defun txt-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "wl-copy" ))
  )
;; (defun txt-paste-function()
;;   (let ((xsel-output (shell-command-to-string "wl-paste")))
;;     (unless (string= (car kill-ring) xsel-output)
;;       xsel-output ))
;;   )
(setq interprogram-cut-function 'txt-cut-function)
;; (setq interprogram-paste-function 'txt-paste-function)

;; rapid-double press to activate key chords
(use-package key-chord
 :ensure t
 :init
 (progn
   (message "Use-package: Key-chord" )
   ;; Max time delay between two key presses to be considered a key chord
   (setq key-chord-two-keys-delay 0.1) ; default 0.1
   ;; Max time delay between two presses of the same key to be considered a key chord.
   ;; Should normally be a little longer than `key-chord-two-keys-delay'.
   (setq key-chord-one-key-delay 0.2) ; default 0.2    
   (key-chord-mode 1)
   ;; k can be bound too
   ;(key-chord-define-global "uu"     'undo-tree-undo)
   ;(key-chord-define-global "kk"     'kill-whole-line)
   (key-chord-define-global "jj"     'avy-goto-word-1)
   (key-chord-define-global "jl"     'avy-goto-line)
   (key-chord-define-global "qq"     'counsel-switch-buffer)
   (key-chord-define-global "qc"     'counsel-org-capture)
   (key-chord-define-global "qb"     'bookmark-set)
   (key-chord-define-global "qj"     'bookmark-jump)
   (key-chord-define-global "qo"     'other-window)
   ;(key-chord-define-global "hh"     'previous-buffer)
   ;(key-chord-define-global "HH"     'next-buffer)
   )
 )

;; AVY is used to jump around within a buffer see key-chords
(use-package avy
  :ensure t
  :defer t
  :init
  (message "Use-package: Avy")
  :config
  (setq avy-background t)
  )

;; Better undo
(use-package undo-tree
  :ensure t
  :defer t
  :init
  (message "Use-package: Undo-tree")
  (global-undo-tree-mode)
  )

;; move focus when splitting a window
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
;; move focus when splitting a window
(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; editorconfig allows specification of tab/space/indent
(use-package editorconfig
  :ensure t
  :defer t
  :delight (editorconfig-mode "EC.")
  :init
  (message "Use-package: EditorConfig")
  :config
  (editorconfig-mode 1)
  )

;; location of my snippets -- has to go before yas-reload-all
(setq-default yas-snippet-dirs '("/home/hewitt/CURRENT/dot.emacs.d/my_snippets"))
;; include yansippet and snippets
(use-package yasnippet
  :delight (yas-minor-mode "YaS.")
  :ensure t
  ;:defer t
  :init
  (message "Use-package: YASnippet")
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

(use-package ivy
  :ensure t
  :delight "Iv."
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (ivy-mode 1)
  :bind (("C-S-s" . isearch-forward)  ;; Keep isearch-forward on Shift-Ctrl-s
         ("C-s" . swiper)             ;; Use swiper for search and reverse search
         ("C-S-r" . isearch-backward) ;; Keep isearch-backward on Shift-Ctrl-r
         ("C-r" . swiper)
	 ("C-y" . counsel-yank-pop)
	 )
  )
;; popup ivy completion in a separate frame top centre instead of in the minibuffer
(use-package ivy-posframe
  :ensure t
  :after ivy
  :delight "Pf."
  :custom-face
  (ivy-posframe-border ((t (:background "#ffffff"))))
  :config
  (ivy-posframe-mode 1)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-height-alist '((t . 10))
        ivy-posframe-parameters '((internal-border-width . 10)))
  (setq ivy-posframe-parameters
      '((left-fringe . 10)
        (right-fringe . 10)))
  (setq ivy-posframe-parameters '((alpha . 0.95)))
  )
;; ivy enhancements to add more information to buffer list
(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
  )
;; adds icons to buffer list
(use-package all-the-icons-ivy-rich
  :ensure t
  :init
  (all-the-icons-ivy-rich-mode 1)
  )

;; I've switched away from Helm in general, but org-wiki still makes use of it
(use-package helm
   :ensure t
   :defer t
)
;; where the package is stored
(add-to-list 'load-path "/home/hewitt/CURRENT/dot.emacs.d/manual_install_packages/org-wiki")
(require 'org-wiki)
;; where my wiki files are stored
(setq org-wiki-location "/home/hewitt/Sync/Org/Wiki")
;; org-wiki-search requires rgrep
(eval-after-load "grep"
  '(grep-compute-defaults))

;; mu4e is part of the "mu" package and sometimes doesn't get
;; found auto-magically. So this points directly to it.
(add-to-list 'load-path "/home/hewitt/local/share/emacs/site-lisp/mu4e")
;; defines mu4e exists, but holds off until needed
(autoload 'mu4e "mu4e" "Launch mu4e and show the main window" t)
;; used for outgoing mail send
(use-package smtpmail
  :ensure t
  :defer t
  :init
  (message "Use-package: SMTPmail")
  (setq message-send-mail-function 'smtpmail-send-it
	user-mail-address "richard.hewitt@manchester.ac.uk"
	smtpmail-default-smtp-server "outgoing.manchester.ac.uk"
	smtpmail-local-domain "manchester.ac.uk"
	smtpmail-smtp-server "outgoing.manchester.ac.uk"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465)
  )
;; this stops errors associated with duplicated UIDs -- LEAVE IT HERE!
(setq mu4e-change-filenames-when-moving t)
;; general mu4e config
(setq mu4e-maildir (expand-file-name "/home/hewitt/CURRENT/mbsyncmail"))
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent Items")
(setq mu4e-trash-folder  "/Trash")
(setq message-signature-file "/home/hewitt/CURRENT/dot.signature")
(setq mu4e-headers-show-thread nil)
(setq mu4e-headers-include-related nil)
(setq mu4e-headers-results-limit 100)
(setq mu4e-mu-binary "/home/hewitt/local/bin/mu")
;; stop mail draft/sent appearing in the recent files list of the dashboard
(add-to-list 'recentf-exclude "\\mbsyncmail\\")
;; how to get mail
(setq mu4e-get-mail-command "mbsync Work"
      ;mu4e-html2text-command "w3m -T text/html"
      mu4e-html2text-command "html2markdown --body-width=70" 
      mu4e-update-interval 300
      mu4e-headers-auto-update t
      ;mu4e-compose-signature-auto-include nil
      )
;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
    '( (:human-date    .  15)    ;; alternatively, use :date
       (:flags         .   6)
       (:from          .  22)
       (:subject       .  nil))  ;; alternatively, use :thread-subject
    )
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"          . ?i)
         ("/Sent Items"     . ?s)
         ("/Deleted Items"  . ?t)
         ("/Drafts"         . ?d))
      )
;; REMOVE BELOW FOR TERMINUAL EMACS
;; show images
(setq mu4e-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types)
  )
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "richard.hewitt@manchester.ac.uk"
    user-mail-address "richard.hewitt@manchester.ac.uk"
    user-full-name  "Rich Hewitt")
;;;; don't save message to Sent Messages, IMAP takes care of this
;; 2019: emails are vanishing with below!
;; (setq mu4e-sent-messages-behavior 'delete)

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode) )
	  )
;;;; https://emacs.stackexchange.com/questions/21723/how-can-i-delete-mu4e-drafts-on-successfully-sending-the-mail
;;;; "As I'm composing mail, mu4e automatically saves drafts to the mu4e-drafts-folder.
;;;; When I send the mail, these drafts persist. I expected mu4e to delete from the folder."
;;;; "If you use offlineimap (like I do) then your drafts likely accumulate because offlineimap syncs
;;;; emacs' #autosave# files (kept in Drafts/cur folder). As offlineimap can only ignore files starting
;;;; with '.' (and it's not configurable) the solution is to change the way draft autosaves are named:
(defun draft-auto-save-buffer-name-handler (operation &rest args)
"for `make-auto-save-file-name' set '.' in front of the file name; do nothing for other operations"
(if
  (and buffer-file-name (eq operation 'make-auto-save-file-name))
  (concat (file-name-directory buffer-file-name)
            "."
            (file-name-nondirectory buffer-file-name))
 (let ((inhibit-file-name-handlers
       (cons 'draft-auto-save-buffer-name-handler
             (and (eq inhibit-file-name-operation operation)
                  inhibit-file-name-handlers)))
      (inhibit-file-name-operation operation))
  (apply operation args))))
(add-to-list 'file-name-handler-alist '("Drafts/cur/" . draft-auto-save-buffer-name-handler))

;; eglot is a simpler alternative to LSP-mode
(use-package eglot
  :ensure t
  :delight (eglot "Eglot.")
  :init
  (message "Use-package: Eglot")
  (add-hook 'c++-mode-hook 'eglot-ensure)
  )
(add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
;; company gives the selection front end for code completion
;; but not the C++-aware backend
(use-package company
  :ensure t
  :delight (company-mode "Co.")
  :bind ("M-/" . company-complete)
  :init
  (progn
    (message "Use-package: Company")
    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (require 'yasnippet)
  ;(setq company-idle-delay 1)
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
			    company-echo-metadata-frontend))
  )

(use-package projectile
  :ensure t
  ; shorten project names in the modeline
  :delight '(:eval (concat "P:" (substring (projectile-project-name) 0 4 ) "." ))
  :defer t
  :init
  (message "Use-package: Projectile")
  :config
  ;(setq projectile-project-search-path '("~/CURRENT/Projects/CppNoddy"
;	 "~/Sync/Org"
;	 "~/CURRENT/dot.emacs.d"
;	 "~/CURRENT/Projects/Research/2020/Big_VWI")
;	)
  (setq projectile-global-mode       t
        projectile-enable-caching    t )
  projectile-globally-ignored-directories
  (append '("build"
	    ".git"
	    ".OLD"
	    "DATA" )
	  projectile-globally-ignored-directories )
  projectile-globally-ignored-files
  (append '(".cpp~"
            ".h~"
            "~")
          projectile-globally-ignored-files)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
  )

;; GIT-GUTTER: SHOW changes relative to git repo
(use-package git-gutter
  :ensure t
  :defer t
  :delight (git-gutter-mode "Gg.")
  :init (message "Use-package: Git-Gutter")
)
(add-hook 'c++-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
(add-hook 'emacs-lisp-mode-hook 'git-gutter-mode)
;; MAGIT
(use-package magit
  :ensure t
  :defer t
  :bind
  ("C-x g" . magit-status)
  :init
  (message "Use-package: Magit installed")
  ;(setq magit-completing-read-function 'ivy-completing-read)
  )

;; F8 : mu4e
(global-set-key (kbd "<f8>") 'mu4e)
;; F9 : org wiki hot key
(global-set-key (kbd "<f9>") 'org-wiki-index)
;; F10 : ORG AGENDA keybinding
(global-set-key (kbd "<f10>") 'org-agenda)
;; F11 is full screen in the Sway WM
;; F12 : turn on the menu bar
(global-set-key (kbd "<f12>") 'menu-bar-mode)
;; C-c e : edit the init.el configuration file
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-set-key (kbd "C-c e") 'config-visit)
;; C-c r : reload the configuration file
(defun config-reload ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el"))
  )
(global-set-key (kbd "C-c r") 'config-reload)
