(use-package amx
  :init
  (message "Use-package: Amx")
  :config
  (setq amx-mode t)
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
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))
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
  :defer t
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
  :defer t
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
