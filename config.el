;;;;
;;;; CONFIGURE MELPA AND GNU ARCHIVES
;;;;
(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities ; prefer ELPA to MELPA
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5 )))
(package-initialize)

;;;;
;;;; CONFIGURE USE-PACKAGE TO AUTOLOAD THINGS : https://github.com/jwiegley/use-package
;;;;
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package) )

;; this is the standard use-package invocation if it is in ~/.emacs.d
;;(eval-when-compile
;;  (require 'use-package) )

;; Keep custom settings in a separate file to not pollute this one
(setq custom-file "/home/hewitt/.emacs.d/custom-settings.el")
(load custom-file t)

;; move backups to stop *~ proliferation
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; have mouse input in the terminal
;; the disadvantage is you need to SHIFT+middle mouse to paste in the terminal
(xterm-mouse-mode 1)
;; Turn off the menu/scroll/toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; this stops the cursor recentering on leaving the page
;; ie. stop scrolling by 0.5 page
(setq scroll-conservatively 101 )
;; replace annoying yes/no with y/n
(fset 'yes-or-no-p 'y-or-n-p)
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

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
;; This COMMAND will reload a buffer if it changes on disk, which is
;; key if editing from multiple machines over long periods.
;(defun revert-buffer-no-confirm ()
;  "Revert buffer without confirmation."
;  (interactive)
;  (revert-buffer :ignore-auto :noconfirm))
(global-auto-revert-mode)

;; setup files ending in “.m4” to open in LaTeX-mode
;; for use in lecture note construction
(add-to-list 'auto-mode-alist '("\\.m4\\'" . latex-mode))
;; my default gnuplot extension
(add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))
;; Octave/Matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; Nix language
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(use-package delight
  ;;ensure t
  :init (message "Use-package: Delight") )
;; delight some basic modes to get rid of modeline content
(delight 'eldoc-mode "" 'eldoc)
(delight 'abbrev-mode "" 'abbrev)

;; dashboard runs at startup by default
(use-package dashboard
  ;;ensure t
  :delight "Dash"
  :init
  (message "Use-package: Dashboard")
  :config
  (setq dashboard-banner-logo-title "Go!")
  (setq dashboard-startup-banner '2) ; 1,2,3 are the text banners
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (agenda . 4)))
  (dashboard-setup-startup-hook)
  ;(dashboard-refresh-buffer)
  )
;; show dashboard on startup for emacsclients when running the daemon
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; Disable all other themes to avoid awkward blending:    
(use-package ef-themes
  :init
  (mapc #'disable-theme custom-enabled-themes)
  ;; Make customisations that affect Emacs faces BEFORE loading a theme
  ;; (any change needs a theme re-load to take effect).
  (setq ef-themes-to-toggle '(ef-day ef-winter))
  ;;:config
  ;; Load the theme of choice:
  ;;(load-theme 'ef-summer :no-confirm)
  ;; Light: `ef-day', `ef-light', `ef-spring', `ef-summer'.
  ;; Dark:  `ef-autumn', `ef-dark', `ef-night', `ef-winter'.

  ;; I set the theme at the end of this configuration because of
  ;; some minor issues with code comments showing as underlined [2022]
  )

(use-package doom-modeline
  ;;ensure t
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
  ; (setq doom-modeline-checker-simple-format t)
  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 6)
  ;; Whether display perspective name or not. Non-nil to display in mode-line.
  ;(setq doom-modeline-persp-name t)
  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-lsp t)  )

(use-package rainbow-delimiters
  ;;ensure t
  :init
  (message "Use-package: Rainbow delimiters")
  :config
  (rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'latex-mode-hook 'rainbow-delimiters-mode) )

(use-package which-key
  ;;ensure t
  :delight 
  :init 
  (message "Use-package: Which-key mode")
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode) )

;; latex  
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'hl-line-mode)
;; programming
(add-hook 'prog-mode-hook 'hl-line-mode)
;; org-mode
(add-hook 'org-mode-hook 'hl-line-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(use-package consult
  ;;ensure t
  :after key-seq
  :init
  (message "Use-package: consult")
  :bind
  ;; see also key-chords elsewhere
  ("C-x b" . consult-buffer)
  ("M-g g" . consult-goto-line)
  ("M-y"   . consult-yank-pop)
  ("C-y"   . yank)
  ("C-s"   . consult-line)
  ("M-g o" . consult-outline))

  ;; define some related chords
  (key-seq-define-global "qq"     'consult-buffer)
  (key-seq-define-global "qb"     'consult-bookmark) ; set or jump
  (key-seq-define-global "ql"     'consult-goto-line)

(use-package consult-notes
  :commands (consult-notes consult-notes-search-in-all-notes)
  :config
  ;(setq consult-notes-file-dir-sources
  ;      '(("Org"         ?o "~/Sync/Org/Denote/")
  ;        ("Journal"      ?j "~/Sync/Org/Journal/")))    
  ;(consult-notes-org-headings-mode)
  (consult-notes-denote-mode))

(use-package vertico
  ;;ensure t
  :custom
  (vertico-cycle t)
  :init
  (message "Use-package: vertico")
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

;; (use-package vertico-posframe
;;   :config
;;   (setq vertico-posframe-border-width 3)
;;   (setq vertico-posframe-mode t)
;;   (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
;;   (setq vertico-posframe-width 100))

 (use-package orderless
  ;;ensure t
  :custom (completion-styles '(orderless)))

(use-package marginalia
  :after vertico
  ;;ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (message "Use-package: marginalia")
  (marginalia-mode))

;; rapid-double press to activate key chords
(use-package key-chord
  ;;ensure t
  :init
  (progn
    (message "Use-package: Key-chord" )
    )
  :config
  ;; Max time delay between two key presses to be considered a key chord
  (setq key-chord-two-keys-delay 0.1) ; default 0.1
  ;; Max time delay between two presses of the same key to be considered a key chord.
  ;; Should normally be a little longer than `key-chord-two-keys-delay'.
  (setq key-chord-one-key-delay 0.2) ; default 0.2    
  (key-chord-mode 1) )

;; NOTE: additional key-chords are defined within other use-package declarations herein.
(use-package key-seq
  ;;ensure t
  :after key-chord
  :init
  (progn
    (message "Use-package: Key-seq" )
    ;(key-seq-define-global "kk"     'kill-whole-line)
    (key-seq-define-global "qs"     'consult-notes-search-in-all-notes) ; search org files
    (key-seq-define-global "qi"     'ibuffer-bs-show) 
    (key-seq-define-global "qw"     'other-window)
    (key-seq-define-global "qt"     'org-babel-tangle)
    (key-seq-define-global "qd"     'org-journal-new-entry)
    (key-seq-define-global "qc"     'org-capture) ) )

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
    ;;ensure t
    :delight (editorconfig-mode "Ec")
    :init
    (message "Use-package: EditorConfig")
    :config
    (editorconfig-mode 1) )
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))

;; location of my snippets -- has to go before yas-reload-all
(setq-default yas-snippet-dirs '("/home/hewitt/.emacs.d/my_snippets"))
;; include yansippet and snippets
(use-package yasnippet
  :delight (yas-minor-mode "YaS")
  ;;ensure t
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
  (define-key yas-minor-mode-map (kbd "M-]") yas-maybe-expand)
  ;; remove default keys for navigation
  (define-key yas-keymap [(tab)]       nil)
  (define-key yas-keymap (kbd "TAB")   nil)
  (define-key yas-keymap [(shift tab)] nil)
  (define-key yas-keymap [backtab]     nil)
  ;; redefine my own keys
  (define-key yas-keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "M-p") 'yas-prev-field)  
  (yas-reload-all) )

(use-package corfu
  ;;ensure t
  :init (message "Use-package: Corfu")
  :hook
  (prog-mode . corfu-mode)
  (latex-mode . corfu-mode)
  (org-mode . corfu-mode) )

;; GIT-GUTTER: SHOW changes relative to git repo
(use-package git-gutter
  ;;ensure t
  :defer t
  :delight (git-gutter-mode "Gg")
  :init (message "Use-package: Git-Gutter")
  :hook
  (prog-mode . git-gutter-mode)
  (org-mode . git-gutter-mode) )

;; eglot is a simpler alternative to LSP-mode
(use-package eglot
  ;;ensure t
  :delight (eglot "Eglot")
  :init
  (message "Use-package: Eglot")
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'latex-mode-hook 'eglot-ensure) 
  :custom
  (add-to-list 'eglot-server-programs '(c++-mode . ("ccls")))
  (add-to-list 'eglot-server-programs '(latex-mode . ("digestif"))) )

;; NIX language mode
(use-package nix-mode
  :delight (nix-mode "Nx")
  :mode "\\.nix\\'" ) 

;; company gives the selection front end for code completion
;; but not the C++-aware backend
(use-package company
  ;;ensure t
  :delight (company-mode "Co")
  :bind ("M-/" . company-complete)
  :init
  (progn
    (message "Use-package: Company")
    (add-hook 'after-init-hook 'global-company-mode) )
  :config
  (require 'yasnippet)
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend 
                            company-echo-metadata-frontend) ) )

;; MAGIT
(use-package magit
  ;;ensure t
  :defer t
  :bind
  ("C-x g" . magit-status)
  :init
  (message "Use-package: Magit installed")
  )

(use-package org
  ;;ensure t
  :after key-seq
  :init
  (message "Use-package: Org") )

;; fancy replace of *** etc
(use-package org-bullets
  ;;ensure t
  :after org
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (message "Use-package: Org-bullets") )

;; replace emphasis with colors in Org files
(setq org-emphasis-alist
       '(("*" my-org-emphasis-bold)
         ("/" my-org-emphasis-italic)
         ("_" underline)
         ("=" org-verbatim verbatim)
         ("~" org-code verbatim)
         ("+" (:strike-through t))))

 (defface my-org-emphasis-bold
   '((default :inherit bold)
     (((class color) (min-colors 88) (background light))
      :foreground "#a60000")
     (((class color) (min-colors 88) (background dark))
      :foreground "#ff8059"))
   "My bold emphasis for Org.")

 (defface my-org-emphasis-italic
   '((default :inherit italic)
     (((class color) (min-colors 88) (background light))
      :foreground "#005e00")
     (((class color) (min-colors 88) (background dark))
      :foreground "#44bc44"))
   "My italic emphasis for Org.")

 (defface my-org-emphasis-underline
   '((default :inherit underline)
     (((class color) (min-colors 88) (background light))
      :foreground "#813e00")
     (((class color) (min-colors 88) (background dark))
      :foreground "#d0bc00"))
   "My underline emphasis for Org.")


   ;; ORG link to mu4e emails -- see mu from https://github.com/djcb/mu
   ;(require 'org-mu4e)
   ;(setq org-mu4e-link-query-in-headers-mode nil)

   ;; custom capture
   (require 'org-capture)
   ;;(define-key global-map "\C-cc" 'org-capture) ; see key-chord/seq
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

   ;; used for org timers?
   (key-seq-define-global "qp"     'org-timer-set-timer)
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
     ;;:ensure t
     :init
     (message "Use-package: gnuplot for babel installed") )
;; languages I work in via babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t) (emacs-lisp . t) (shell . t) (python . t)))
;; stop it asking if I'm sure about evaluation
(setq org-confirm-babel-evaluate nil)

(defun my-tab-related-stuff ()
  (setq indent-tabs-mode nil)
  ;;(setq tab-stop-list (number-sequence 4 200 4))
  (setq tab-width 2)
  ;;(setq indent-line-function 'insert-tab)
  )

(add-hook 'org-mode-hook 'my-tab-related-stuff)

(require 'denote)

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/Sync/Org/Denote/"))
(setq denote-known-keywords '("research" "admin" "industry" "teaching" "home" "attachment"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(title keywords))

;; We allow multi-word keywords by default.  The author's personal
;; preference is for single-word keywords for a more rigid workflow.
(setq denote-allow-multi-word-keywords t)

(setq denote-date-format nil) ; read doc string

;; You will not need to `require' all those individually once the
;; package is available.
;;(require 'denote-retrieve)
;;(require 'denote-link)

;; By default, we fontify backlinks in their bespoke buffer.
(setq denote-link-fontify-backlinks t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;;(require 'denote-dired)
(setq denote-dired-rename-expert nil)

;; We use different ways to specify a path for demo purposes.
;;(setq denote-dired-directories
;;      (list denote-directory
;;            (thread-last denote-directory (expand-file-name "attachments"))
;;            (expand-file-name "~/Documents/books")))

;; Generic (great if you rename files Denote-style in lots of places):
;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Denote does not define any key bindings.  This is for the user to
;; decide.  For example:
(let ((map global-map))
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-link-add-links)
  (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
  (define-key map (kbd "C-c n b") #'denote-link-backlinks)
  ;; Note that `denote-dired-rename-file' can work from any context, not
  ;; just Dired bufffers.  That is why we bind it here to the
  ;; `global-map'.
  (define-key map (kbd "C-c n r") #'denote-dired-rename-file))

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
  ;;ensure t
  :init
  (message "Use-package: Org-journal")
  :config
  (setq org-journal-dir "~/Sync/Org/Journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y_%m_%d"
        org-journal-time-prefix "  - "
        org-journal-time-format nil
        org-journal-file-type 'monthly)  )

;; pdf tools for organising and annotating PDF
(use-package pdf-tools
  ;;ensure t
  :config
  (pdf-tools-install) )

;; Feb 2023 : mu is not installed via Nix so no need to specialise
;; mu4e is part of the "mu" package and sometimes doesn't get
;; found auto-magically. So this points directly to it.
;; (add-to-list 'load-path "/home/hewitt/local/share/emacs/site-lisp/mu4e")

;; defines mu4e exists, but holds off until needed
(autoload 'mu4e "mu4e" "Launch mu4e and show the main window" t)

;; used for outgoing mail send
(use-package smtpmail
  ;;ensure t
  :defer t
  :init
  (message "Use-package: SMTPmail")
  (setq message-send-mail-function 'smtpmail-send-it
        user-mail-address "richard.hewitt@manchester.ac.uk"
        ;;smtpmail-default-smtp-server "outgoing.manchester.ac.uk"
        smtpmail-default-smtp-server "localhost" ; davmail runs locally
        ;;smtpmail-local-domain "manchester.ac.uk"
        smtpmail-smtp-server "localhost"
        ;;smtpmail-stream-type 'starttls
        smtpmail-smtp-service 1025) )

;; 2018 : this stops errors associated with duplicated UIDs -- LEAVE IT HERE!
(setq mu4e-change-filenames-when-moving t)
;; general mu4e config
(setq mu4e-maildir (expand-file-name "/home/hewitt/CURRENT/mbsyncmail"))
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent") ; they still seem to appear in O365 despite this not being "Sent Items"
(setq mu4e-trash-folder  "/Deleted Items") ; I don't sync Deleted Items & largely do permanent delete "D" rather than move to trash "d"
(setq message-signature-file "/home/hewitt/CURRENT/dot.signature")
(setq mu4e-headers-show-thread nil)
(setq mu4e-headers-include-related nil)
(setq mu4e-headers-results-limit 200)
(setq mu4e-mu-binary (executable-find "mu"))
;; stop mail draft/sent appearing in the recent files list of the dashboard
;;(add-to-list 'recentf-exclude "\\mbsyncmail\\")
;; how to get mail
(setq mu4e-get-mail-command "mbsync Work"
      mu4e-html2text-command "w3m -T text/html"
      ;;mu4e-html2text-command "html2markdown --body-width=72" 
      ;;mu4e-update-interval 300
      ;;mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include t)

;; the headers to show 
;; in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
      '((:human-date    .  15)    ;; alternatively, use :date
        (:flags        .   6)
        (:from         .  22)
        (:subject      .  nil))  ;; alternatively, use :thread-subject
      )
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"          . ?i)
         ("/Sent"           . ?s)
         ("/Deleted Items"  . ?t)
         ("/Drafts"         . ?d)) )
;; REMOVE BELOW FOR TERMINAL EMACS
;; show images
(setq mu4e-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types) )
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "richard.hewitt@manchester.ac.uk"
      user-mail-address "richard.hewitt@manchester.ac.uk"
      user-full-name  "Rich Hewitt")
(setq mu4e-sent-messages-behavior 'sent)

;; spell check during compose
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)
            ;; turn off autosave, otherwise we end up with multiple
            ;; versions of sent/draft mail being sync'd
            (auto-save-mode -1) ) )

(use-package age
  ;;; :quelpa (age :fetcher github :repo "anticomputer/age.el") 
  :ensure t
  :demand
  :custom
  (age-program "rage")
  (age-default-identity "~/CURRENT/AGE/age-yubikey-identity-bb978fd1.txt")
  (age-default-recipient
   '("~/CURRENT/AGE/backupKey.pub"
     "~/CURRENT/AGE/age-yubikey-identity-bb978fd1.pub"))
  :config
  (setq age-armor nil) ;; don't convert to ASCII so I can see the key headers
  (age-file-enable))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   ;; SELECTION
   '("," . meow-inner-of-thing) 
   '("." . meow-bounds-of-thing)
   '("g" . meow-cancel-selection)
   '("H" . meow-left-expand)
   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   '("L" . meow-right-expand)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   ;; MOVEMENT
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   ;; EDIT
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("i" . meow-insert)
   '("a" . meow-append)
   '("I" . meow-open-above)
   '("A" . meow-open-below)
   '("s" . meow-kill)
   ;;
   ;; SEARCH
   '("f" . meow-find)
   '("G" . meow-grab)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   ;;'("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

   (require 'meow)
   (meow-setup)
   (meow-global-mode 1)

;; simple prefix key launcher
(global-set-key (kbd "C-c h m") 'mu4e)
(global-set-key (kbd "C-c h a") 'org-agenda)
;; C-c h e : edit the init.el configuration file
(defun config-visit ()
  (interactive)
  (find-file "~/CURRENT/NixConfig/outOfStore/.emacs.d/config.org") )
(global-set-key (kbd "C-c h e") 'config-visit)
;; C-c h e : edit the init.el configuration file
(defun todo-visit ()
  (interactive)
  (find-file "~/Sync/Org/Todo.org") )
(global-set-key (kbd "C-c h t") 'todo-visit)

;; load default theme last.
(load-theme 'ef-duo-dark :no-confirm)

;; because we use daemon/client we have to initiate the posframe
;; mode only once a frame is made
;;(add-hook 'after-make-frame-functions
;;  (lambda (frame)
;;    (select-frame frame)
;;    (when (display-graphic-p frame)
;;      (vertico-posframe-mode 1))))
