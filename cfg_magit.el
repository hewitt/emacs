;;;; GIT-GUTTER: SHOW changes relative to git repo
(use-package git-gutter
  :ensure t
  :defer t
  :delight (git-gutter-mode "GG.")
  :init (message "Use-package: Git-Gutter")
)
(add-hook 'c++-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
(add-hook 'emacs-lisp-mode-hook 'git-gutter-mode)


;;;; MAGIT
(use-package magit
  :ensure t
  ;:defer t
  :bind
  ("C-x g" . magit-status)
  :init
  (message "Use-package: Magit installed")
  ;(setq magit-completing-read-function 'ivy-completing-read)
  )
