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

;;;;
;;;; REQUIRED FOR SUB-FILE LOADING
;;;;
;;;; define where stuff is stored
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")
	)
  )
;; define a function to load sub-parts of the init
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir))
  )

;;;;
;;;; Shortcut to VISIT THIS FILE (C-c e) & reload it (C-c r)
;;;;
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-set-key (kbd "C-c e") 'config-visit)
(defun config-reload ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el"))
  )
(global-set-key (kbd "C-c r") 'config-reload)


;;;;
;;;; Machine identification
;;;;
(defun system-is-Blasius ()
  (interactive)
  "True for Blasius laptop"
  (string-equal system-name "Blasius"))
(defun system-is-Orthanc ()
  (interactive)
  "True for Orthanc desktop"
  (string-equal system-name "Orthanc"))


;; MOVE BACKUPS TO STOP *~ PROLIFERATION
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; have mouse input in the terminal
;; -- disadvantage is you need to SHIFT+middle mouse to paste
(xterm-mouse-mode 1)
;; replace annoying yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; don't end sentences with a single space
(setq sentence-end-double-space nil)
;; From Prelude: reduce the frequency of garbage collection by making it happen on
;; each 5MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 5000000)
;; warn when opening files bigger than 10MB
(setq large-file-warning-threshold 10000000)


;;;;------------------------------------------------------------
;;;; NOW LOAD MY SUB CONFIGURATIONS
;;;;------------------------------------------------------------
;; faces, theme, scrollbar, helm, snippets etc
(load-user-file "cfg_looknfeel.el")
;; mu4e config
(load-user-file "cfg_mu4e.el")
;; org mode config
(load-user-file "cfg_org.el")
;; projectile config
(load-user-file "cfg_projectile.el")
;; babel config
(load-user-file "cfg_babel.el")
;; magit config
(load-user-file "cfg_magit.el")
;; C++ coding configs
(load-user-file "cfg_coding.el")
;; manually installed packages are here
(load-user-file "cfg_manual.el")

;(set-face-background 'hl-line "#333333")

(global-set-key (kbd "<f8>") 'mu4e)
(global-set-key (kbd "<f12>") 'menu-bar-mode)










(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-safe-themes
   '("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "b3697d12fb7c087e1337432be92026b5fd218e7e43277918c0fce680d573a90c" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "63df625509cec7b2b7deba1d48ef07ffbee99c735a95d130ad9d1fc3df8b9a34" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "0736a8e34702a67d84e32e2af90145ed19824f661776a0e966cea62aa1943a6e" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "559b28ae6deb74713fee9064e7ece54cb71ba645f44acbf81ad7916a4f947815" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "72fda75af7caddec17ba9b49d2f99703c20a5f5f5c4dcec641d34a0b83569e88" "ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" default))
 '(fci-rule-color "#6272a4")
 '(helm-completion-style 'emacs)
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(objed-cursor-color "#ff5555")
 '(package-selected-packages
   '(git-gutter-fringe color-theme-sanityinc-solarized chocolate-theme company-posframe dracula-theme org-superstar mu4e-conversation all-the-icons which-key helm-projectile projectile treemacs helm-posframe dashboard doom-themes use-package))
 '(pdf-view-midnight-colors (cons "#f8f8f2" "#282a36"))
 '(rustic-ansi-faces
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(tool-bar-mode nil)
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term SS05" :foundry "CYEL" :slant normal :weight normal :height 130 :width normal)))))
 ;;'(default ((t (:family "RobotoMono" :foundry "CYEL" :slant normal :weight normal :height 130 :width normal)))))
 ;;'(default ((t (:family "FiraCode" :foundry "CYEL" :slant normal :weight normal :height 130 :width normal)))))
