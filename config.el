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
;; Turn off the menu/scroll/toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
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

;; setup files ending in “.m4” to open in LaTeX-mode
;; for use in lecture note construction
(add-to-list 'auto-mode-alist '("\\.m4\\'" . latex-mode))
(add-hook 'latex-mode-hook 'flyspell-mode)

(use-package delight
  :ensure t
  ;:no-require t
  :init (message "Use-package: Delight")
  )
(delight 'eldoc-mode "Eld" 'eldoc)
(delight 'undo-tree-mode "Ut" 'undo-tree)
(delight 'abbrev-mode "Ab" 'abbrev)

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
(use-package modus-vivendi-theme
  :ensure t
  :init
    ;;  customisations must be defined before the theme is loaded
    ;; NOTE: Everything is disabled by default.
    (setq modus-vivendi-theme-slanted-constructs t
      modus-vivendi-theme-bold-constructs t    
      modus-vivendi-theme-fringes 'subtle ; {nil,'subtle,'intense}
      modus-vivendi-theme-3d-modeline t        
      modus-vivendi-theme-faint-syntax t       
      modus-vivendi-theme-intense-hl-line t    
      modus-vivendi-theme-intense-paren-match t
      modus-vivendi-theme-prompts 'subtle ; {nil,'subtle,'intense}
      modus-vivendi-theme-completions 'moderate ; {nil,'moderate,'opinionated}
      modus-vivendi-theme-diffs nil ; {nil,'desaturated,'fg-only}
      modus-vivendi-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
      modus-vivendi-theme-variable-pitch-headings t
      modus-vivendi-theme-rainbow-headings t
      modus-vivendi-theme-section-headings 'nil
      modus-vivendi-theme-scale-headings t
      modus-vivendi-theme-scale-1 1.05
      modus-vivendi-theme-scale-2 1.1
      modus-vivendi-theme-scale-3 1.15
      modus-vivendi-theme-scale-4 1.2
      modus-vivendi-theme-scale-5 1.3)
  :config
    (load-theme 'modus-vivendi t)      
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

(use-package which-key
  :ensure t
  :delight
  :init 
  (message "Use-package: Which-key mode")
  :config
  (which-key-mode)
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
    ;(key-chord-define-global "kk"     'kill-whole-line)
    ;(key-chord-define-global "qw"     'avy-goto-word-1)
    ;(key-chord-define-global "qs"     'deft)
    ;(key-chord-define-global "qt"     'org-babel-tangle)
    ;(key-chord-define-global "qq"     'counsel-switch-buffer)
    ;(key-chord-define-global "qc"     'counsel-org-capture)
    ;(key-chord-define-global "qb"     'bookmark-set)
    ;(key-chord-define-global "qj"     'bookmark-jump)
    ;(key-chord-define-global "qo"     'other-window)
    ;(key-chord-define-global "qd"     'org-journal-new-entry)
    )
  )

;; I like key-chord but the order of the keys is ignored ie. qs is equivalent to sq
;; instead key-seq checks the order -- but relies on key-chord-mode still
(use-package key-seq
  :ensure t
  :after key-chord
  :init
  (progn
    (message "Use-package: Key-seq" )
    (key-seq-define-global "qd" 'dired)
    (key-seq-define-global "kk"     'kill-whole-line)
    (key-seq-define-global "qw"     'avy-goto-word-1)
    (key-seq-define-global "qs"     'deft)
    (key-seq-define-global "qt"     'org-babel-tangle)
    (key-seq-define-global "qq"     'counsel-switch-buffer)
    (key-seq-define-global "qc"     'counsel-org-capture)
    (key-seq-define-global "qb"     'bookmark-set)
    (key-seq-define-global "qj"     'bookmark-jump)
    (key-seq-define-global "qo"     'other-window)
    (key-seq-define-global "qd"     'org-journal-new-entry)
    )
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
  :delight (editorconfig-mode "Ec")
  :init
  (message "Use-package: EditorConfig")
  :config
  (editorconfig-mode 1)
  )

;; location of my snippets -- has to go before yas-reload-all
(setq-default yas-snippet-dirs '("/home/hewitt/CURRENT/dot.emacs.d/my_snippets"))
;; include yansippet and snippets
(use-package yasnippet
  :delight (yas-minor-mode "YaS")
  :ensure t
  :init
  (message "Use-package: YASnippet")
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; hooks for YASnippet in Latex and C++;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'c++-mode-hook 'yas-minor-mode)
  (add-hook 'latex-mode-hook 'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
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
  :delight "Iv"
  :init
  (message "Use-package: Ivy")
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (ivy-mode 1)
  :bind (("C-S-s" . isearch-forward)  ;; Keep simpler isearch-forward on Shift-Ctrl-s
         ("C-s" . swiper)             ;; Use more intrusive swiper for search and reverse search
         ("C-S-r" . isearch-backward) ;; Keep simpler isearch-backward on Shift-Ctrl-r 
         ("C-r" . swiper)             ;; Use more intrusive swiper for search and reverse search
             ("C-y" . counsel-yank-pop)   ;; Use more intrusive pop-up list to yank
         ("M-x" . counsel-M-x)        ;; More descriptive M-x
         ("C-h v" . counsel-describe-variable) ;; Slightly fancier lookup
         ("C-h f" . counsel-describe-function) ;; Slightly fancier lookup
         ("C-h o" . counsel-describe-symbol)   ;; Slightly fancier lookup
         )
  )
;; popup ivy completion in a separate frame top centre instead of in the minibuffer
(use-package ivy-posframe
  :ensure t
  :after ivy
  :delight 
  :config
  (ivy-posframe-mode 1)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 10)))
  (setq ivy-posframe-parameters
      '((left-fringe . 5)
        (right-fringe . 5)))
  (setq ivy-posframe-parameters '((alpha . 0.90)))
  )
;; ivy enhancements to add more information to buffer list
(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1)
  )
;; adds icons to buffer list
(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1)
  )

(use-package prescient
  :ensure t
  :init
  (message "Use-package: prescient")
  :config
  (prescient-persist-mode 1)
  )
(use-package ivy-prescient
  :ensure t
  :after (counsel prescient)
  :config
  (ivy-prescient-mode 1)
  )
(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode 1)
  )

;; eglot is a simpler alternative to LSP-mode
(use-package eglot
  :ensure t
  :delight (eglot "Eglot")
  :init
  (message "Use-package: Eglot")
  (add-hook 'c++-mode-hook 'eglot-ensure)
  )
(add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))

;; company gives the selection front end for code completion
;; but not the C++-aware backend
(use-package company
  :ensure t
  :delight (company-mode "Co")
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
        company-echo-metadata-frontend)
        )
  )

(use-package org
  :ensure t
  :init
  (message "Use-package: Org")
  )

;; fancy replace of *** etc
(use-package org-bullets
  :ensure t
  :after org
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (message "Use-package: Org-bullets")
  )

;; ORG link to mu4e -- see mu from https://github.com/djcb/mu
(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

;; custom capture
(require 'org-capture)
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/Sync/Org/Todo.org" "Inbox")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("z" "Zoom meeting" entry (file+headline "~/Sync/Org/Todo.org" "Meetings")
         "* TODO Zoom, %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n"
         :empty-lines 1))
      )

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

;; highlight the current line in the agenda
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; define the number of days to show in the agenda
(setq org-agenda-span 14
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")

;; default duration of events
(setq org-agenda-default-appointment-duration 60)

;; function for below
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil))
  )

;; custom agenda view
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))))

;; calendar export
(setq org-icalendar-alarm-time 45)
;; This makes sure to-do items as a category can show up on the calendar
(setq org-icalendar-include-todo nil)
;; dont include the body
(setq org-icalendar-include-body nil)
;; This ensures all org "deadlines" show up, and show up as due dates
;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
;; This ensures "scheduled" org items show up, and show up as start times
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
(setq org-icalendar-categories '(all-tags))
;; this makes repeated scheduled tasks NOT show after the deadline is passed
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)

;; my own function to export to .ics
(defun reh/export-to-ics ()
  "Routine that dumps Todo.org to Todo.ics in Syncthing"
  (interactive)
  ;(shell-command "rm /home/hewitt/Sync/Org/Todo.ics")
  (with-current-buffer (find-file-noselect "/home/hewitt/Sync/Org/Todo.org")
    (rename-file (org-icalendar-export-to-ics)
                 "/home/hewitt/Sync/Org/Todo.ics" t)
    (message "Exported Todo.org to Todo.ics"))
  )

;; Annoying output littered with S
(defun reh/replaceS ()
  (interactive)
  (shell-command "sed -i -e \'s/SUMMARY:S:/SUMMARY:/g\' /home/hewitt/Sync/Org/Todo.ics")
  )

(if (system-is-Orthanc)
;; ONLY RUN THIS ON THE OFFICE MACHINE -- to avoid conflicted copies of .ics file
    ( progn (message "Machine is Orthanc" )
            (message "Writing Org calendar to ics every 30 minutes" )
            (run-with-timer 60 1800 'reh/export-to-ics)
            (run-with-timer 90 1800 'reh/replaceS) )
  )
(if (system-is-Blasius)
    ( progn (message "Machine is Blasius" )
            (message "Not running the .ics generator" ) )
  )

(use-package gnuplot
  :ensure t
  :init
  (message "Use-package: gnuplot for babel installed")
  )
  ;; languages I work in via babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t) (emacs-lisp . t) (shell . t) (python . t)))
  ;; stop it asking if I'm sure about evaluation
  (setq org-confirm-babel-evaluate nil)

(use-package org-roam
  :ensure t
  :delight "Or"
  :after org
  :init
  (message "Use-package: Org-roam")
  :config
  (setq org-roam-directory "~/Sync/Org/Roam")
  (setq org-roam-graph-viewer "/usr/bin/eog")
  (setq org-ellipsis "▾")
  (setq org-roam-ref-capture-templates
    '(
      ("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t) )
      )
  )

(use-package company-org-roam
  :ensure t
  :after org-roam
  ;; You may want to pin in case the version from stable.melpa.org is not working 
  ; :pin melpa
  :config
  (push 'company-org-roam company-backends)
  )

(use-package org-roam-server
  :ensure t
  :after org-roam
  :init
  (message "Use-package: Org-roam-server")
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "doc" "docx" "mp4")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20
        org-roam-server-mode nil) ; default to off
  )

(use-package org-journal
  :ensure t
  :init
  (message "Use-package: Org-journal")
  :config
  (setq org-journal-dir "~/Sync/Org/Roam/Journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y_%m_%d"
        org-journal-time-prefix "  - "
        org-journal-time-format nil
        org-journal-file-type 'monthly)
  )

;;
;; custom faces/colours are in custom-setting.el
;;
;(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'after-init-hook 'org-roam-mode)

(use-package deft
   :ensure t
   :after org
   :init
   (message "Use-package: Deft")
   :config
   (setq deft-recursive t)
   (setq deft-default-extension "org")
   (setq deft-directory "~/Sync/Org/Roam")
   )

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/CURRENT/dot.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)

  ; see https://protesilaos.com/dotemacs/
  (defun prot/elfeed-show-eww (&optional link)
    "Browse current `elfeed' entry link in `eww'.
Only show the readable part once the website loads.  This can
fail on poorly-designed websites."
    (interactive)
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           (link (if link link (elfeed-entry-link entry))))
      (eww link)
      (add-hook 'eww-after-render-hook 'eww-readable nil t))
      ) ;close defun
      
  :bind
  (("C-c f" . elfeed)
         :map elfeed-search-mode-map
        ("e" . prot/elfeed-show-eww)
        )
)

;; pdf tools for organising and annotating PDF
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  )

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
;; better only use that for the last field.
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

;; F7 : elfeed
(global-set-key (kbd "<f7>") 'elfeed)
;; F8 : mu4e
(global-set-key (kbd "<f8>") 'mu4e)
;; F9 : org wiki hot key
(global-set-key (kbd "<f9>") 'org-roam)
;; F10 : ORG AGENDA keybinding
(global-set-key (kbd "<f10>") 'org-agenda)
;; F11 is full screen in the Sway WM
;; F12 : turn on the menu bar
(global-set-key (kbd "<f12>") 'menu-bar-mode)
;; C-c e : edit the init.el configuration file
(defun config-visit ()
  (interactive)
  (find-file "~/CURRENT/dot.emacs.d/config.org")
  )
(global-set-key (kbd "C-c e") 'config-visit)
;; C-c r : reload the configuration file
(defun config-reload ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el"))
  )
(global-set-key (kbd "C-c r") 'config-reload)
