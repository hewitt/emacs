(use-package projectile
  :ensure t
  ; shorten project names in the modeline
  :delight '(:eval (concat "P:" (substring (projectile-project-name) 0 4 ) "." ))
  ;:defer t
  :init
  (message "Use-package: Projectile")
  :config
  ;(setq projectile-project-search-path '("~/CURRENT/Projects/CppNoddy"
;	 "~/Sync/Org"
;	 "~/CURRENT/dot.emacs.d"
;	 "~/CURRENT/Projects/Research/2020/Big_VWI")
;	)
  (setq projectile-global-mode       t
        projectile-completion-system 'helm
        projectile-enable-caching    t )
  projectile-globally-ignored-directories
  (append '("build"
	    ".git"
	    ".OLD"
	    "DATA" )
	  projectile-globally-ignored-directories )
  projectile-globally-ignored-files
  (append '(".cpp~"
            ".h~"
            "~")
          projectile-globally-ignored-files)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
  )

;(use-package helm-projectile
;  :ensure t
;  ;:defer t
;  :init
;  (message "Use-package: helm-projectile")
;  :config
;  (setq helm-projectile-on t)
;  )

