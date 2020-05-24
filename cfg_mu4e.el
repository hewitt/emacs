;;;;-------------------------------------
;;;; MORE MU4E STUFF BELOW HERE
;;;;-------------------------------------

;; mu4e is part of the "mu" package and sometimes doesn't get
;; found auto-magically. So this points directly to it.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
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

;; how to get mail
(setq mu4e-get-mail-command "mbsync Work"
      ;mu4e-html2text-command "w3m -T text/html"
      mu4e-html2text-command "html2markdown --body-width=70" 
      mu4e-update-interval 300
      mu4e-headers-auto-update t
      ;;      mu4e-compose-signature-auto-include nil
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

;;;; BELOW REMOVED FOR TERMINUAL EMACS
;;;; show images
;;(setq mu4e-show-images t)
;;;; use imagemagick, if available
;;(when (fboundp 'imagemagick-register-types)
;;  (imagemagick-register-types)
;;  )

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "richard.hewitt@manchester.ac.uk"
    user-mail-address "richard.hewitt@manchester.ac.uk"
    user-full-name  "Rich Hewitt")

;;;; don't save message to Sent Messages, IMAP takes care of this
;; emails are vanishing with below!
;;(setq mu4e-sent-messages-behavior 'delete)

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
