;;;;--------------------------------
;;;; ORG MODE CONFIG
;;;;--------------------------------
(use-package org
  :ensure t
  :init
  (message "Use-package: org")
  )

;; org wiki hot key
(global-set-key [f9] 'org-wiki-index)

;; fancy replace of *** etc
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (message "Use-package: org-bullets")
  )

;; ORG link to mu4e -- see mu from https://github.com/djcb/mu
(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

;; CUSTOM CAPTURE 
(require 'org-capture)
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/Sync/Org/Todo.org" "Inbox")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("k" "Note" entry (file+headline "~/Sync/Org/Notes.org" "Inbox")
         "* NOTE %?\nTAKEN: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

;; ORG AGENDA keybinding
(global-set-key (kbd "<f10>") 'org-agenda)

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

;; highlight the current line in the agenda
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; define the number of days to show in the agenda
(setq org-agenda-span 10
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

; auto-start agenda
;(add-hook 'after-init-hook (lambda () (org-agenda nil "c")))

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
  "Routine that dumps Todo.org to Todo.ics in dropbox"
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

