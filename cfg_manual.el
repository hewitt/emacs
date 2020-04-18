;;;;-------------------------------------
;;;; MANUAL INSTALLATION STUFF BELOW HERE
;;;;-------------------------------------

(add-to-list 'load-path "/home/hewitt/CURRENT/dot.emacs.d/manual_install_packages/org-wiki")
(require 'org-wiki)
(setq org-wiki-location "/home/hewitt/Sync/Org/Wiki")

;; org-wiki-search requires rgrep
(eval-after-load "grep"
  '(grep-compute-defaults))
