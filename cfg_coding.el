;;;; eglot is a simpler alternative to LSP-mode
(use-package eglot
  :ensure t
  :delight (eglot "Eglot.")
  :init
  (message "Use-package: Eglot")
  (add-hook 'c++-mode-hook 'eglot-ensure)
  )

;;;; Blasius has a newer O/S with later version of clangd
;(if (system-is-Blasius)
;    (add-to-list 'eglot-server-programs '(c++-mode . ("clangd-9")))
;  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd-8")))
;  )

;;;; company gives the selection front end for code completion
;;;; but not the C++-aware backend
(use-package company
  :ensure t
  :delight (company-mode "Co.")
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
			    company-echo-metadata-frontend))
  )


