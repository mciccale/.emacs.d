;;; `misc-packages-config.el' --- Some miscellaneous packages

(use-package magit
  :ensure t
  :straight t
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)
  :defer t)

(use-package olivetti
  :ensure t
  :straight t
  :defer t
  :custom
  (olivetti-body-width 90))

(use-package simple-httpd
  :ensure t
  :straight t
  :defer t)

(use-package vterm
  :ensure t
  :straight t)

(use-package hackernews
  :ensure t
  :straight t
  :defer t)

;; Provide ourselves
(provide 'misc-packages-config)

;;; `misc-packages-config.el' ends here
