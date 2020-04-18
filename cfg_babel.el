;; active Babel languages
(use-package gnuplot
  :ensure t
  :defer t
  :init
  (message "Use-package: gnuplot for babel installed")
  :config
  ;; languages I work in via babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t) (emacs-lisp . t) (shell . t) (python . t)))
  ;; stop it asking if I'm sure about evaluation
  (setq org-confirm-babel-evaluate nil)
  )

