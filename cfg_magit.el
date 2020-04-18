;;;; GIT-GUTTER: SHOW changes relative to git repo
(use-package git-gutter
  :ensure t
  :defer t
  :delight (git-gutter-mode " GG")
  :init (message "Use-package: git-gutter")
  :hook
  ;; always run git gutter
  (c++-mode-hook . git-gutter-mode)
  (python-mode-hook . git-gutter-mode)
  (lisp-mode-hook . git-gutter-mode)
  )
;  (add-hook 'c++-mode-hook 'git-gutter-mode)
;  (add-hook 'python-mode-hook 'git-gutter-mode)
;  (add-hook 'lisp-mode-hook 'git-gutter-mode)


;;;; MAGIT
(use-package magit
  :ensure t
  ;:defer t
  :bind
  ("C-x g" . magit-status)
  :init
  (message "Use-package: magit installed")
  ;(setq magit-completing-read-function 'ivy-completing-read)
  )
