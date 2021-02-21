;;;;
;;;; CONFIGURE MELPA AND GNU ARCHIVES
;;;;
(require 'package)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(setq package-archives
    '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
      ("MELPA"        . "https://melpa.org/packages/"))
    package-archive-priorities
    '(("GNU ELPA"     . 10)
      ("MELPA" . 5)))
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
;; always follow the symlink
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
;; my default gnuplot extension
(add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))
;; Octave/Matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; defaullt to spelll check in latex
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

(set-face-attribute 'default nil
:family "Iosevka SS05" :height 130 :weight 'normal :width 'expanded)
(set-face-attribute 'variable-pitch nil
:family "Iosevka" :height 1.0 :weight 'normal)
(set-face-attribute 'fixed-pitch nil
:family "Iosevka Fixed" :height 1.0 :weight 'normal :width 'expanded)
(setq hl-line-mode t)
;;
(use-package modus-themes
:ensure
:init
;; Add all your customizations prior to loading the themes
(setq modus-vivendi-theme-slanted-constructs t ; allow italics
      modus-vivendi-theme-bold-constructs t ; allow bold font use
      modus-themes-syntax 'yellow-comments-green-strings ; highlighting
      modus-themes-mode-line 'borderless ; add vertical separators
      modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
      modus-themes-intense-hl-line t ; stronger hl-line-mode
      modus-themes-org-blocks 'rainbow ; {nil,'greyscale,'rainbow}
      modus-themes-paren-match 'intense-bold ;
      modus-themes-scale-headings t ; scale heading text
      modus-themes-rainbow-headings t
      modus-themes-scale-1 1.05
      modus-themes-scale-2 1.1
      modus-themes-scale-3 1.15
      modus-themes-scale-4 1.2
      modus-themes-scale-5 1.3
      ;;
      ;modus-vivendi-theme-fringes 'intense ; {nil,'subtle,'intense}
      ;modus-vivendi-theme-3d-modeline t        
      ;modus-vivendi-theme-faint-syntax t       
      ;modus-vivendi-theme-intense-hl-line t    
      ;modus-vivendi-theme-intense-paren-match t
      ;modus-vivendi-theme-prompts 'intense ; {nil,'subtle,'intense}
      ;modus-vivendi-theme-diffs nil ; {nil,'desaturated,'fg-only}
      ;modus-vivendi-theme-org-blocks 'rainbow 
      ;modus-vivendi-theme-variable-pitch-headings t
      ;modus-vivendi-theme-rainbow-headings t
      ;modus-vivendi-theme-section-headings 'nil
      ;modus-vivendi-theme-scale-headings t
      ;modus-vivendi-theme-scale-1 1.05
      ;modus-vivendi-theme-scale-2 1.1
      ;modus-vivendi-theme-scale-3 1.15
      ;modus-vivendi-theme-scale-4 1.2
      ;modus-vivendi-theme-scale-5 1.3)
      )
;; Load the theme files before enabling a theme
(modus-themes-load-themes)
:config
;; Load the theme of your choice:
;;(modus-themes-load-operandi) ;; OR 
(modus-themes-load-vivendi)
:bind ("<f5>" . modus-themes-toggle))

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

(use-package mini-frame
  :ensure t
  :init
  (message "Use-package: mini-frame")
  :config
  (mini-frame-mode 1)
  :custom
  (x-gtk-resize-child-frames 'resize-mod)
  (mini-frame-resize nil)
  )
(custom-set-variables
  '(mini-frame-show-parameters
  '((top . 10)
  (width . 0.7)
  (left . 0.5)
  (height . 0.5))))
  (setq x-gtk-resize-child-frames 'resize-mode)

(use-package prescient
  :ensure t
  :init
  (message "Use-package: prescient")
  :config
  ; store across restarts
  (prescient-persist-mode 1)
  )
(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode 1)
  )

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
)
(use-package consult
  :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("M-g g" . consult-goto-line)
  ("M-y"   . consult-yank-pop)
  ("C-y"   . consult-yank)
  ("C-s"   . consult-line)
  ("M-g o" . consult-outline)
)
(use-package selectrum
  :ensure t
  :after (consult marginalia)
  :init (message "Use-package: selectrum")
  :config
  (selectrum-mode 1)
)
(use-package selectrum-prescient
  :ensure t
  :init (message "Use-package: selectrum-prescient")
  :after (prescient selectrum)
  :config
  (selectrum-prescient-mode 1)
)
(marginalia-cycle)

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
    ;(key-seq-define-global "qd" 'dired)
    (key-seq-define-global "kk"     'kill-whole-line)
    (key-seq-define-global "qs"     'deft)
    (key-seq-define-global "qt"     'org-babel-tangle)
    (key-seq-define-global "qq"     'consult-buffer)
    (key-seq-define-global "qc"     'org-capture)
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

;; eglot is a simpler alternative to LSP-mode
(use-package eglot
  :ensure t
  :delight (eglot "Eglot")
  :init
  (message "Use-package: Eglot")
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'latex-mode-hook 'eglot-ensure)

  )
(add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
(add-to-list 'eglot-server-programs '(latex-mode . ("digestif")))

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

;; org-latex-export quotes are nasty, so replace them here
;(setq org-export-with-smart-quotes t)
;(add-to-list 'org-export-smart-quotes-alist 
;           '("am"
;             (primary-opening   :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"  :texinfo "``")
;             (primary-closing   :utf-8 "”" :html "&rdquo;" :latex "}"           :texinfo "''")
;             (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
;             (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"           :texinfo "'")
;             (apostrophe        :utf-8 "’" :html "&rsquo;")))

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
; doesn't start by default
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
        org-roam-server-mode nil)
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
