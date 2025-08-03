;;; org-config.el --- Org-mode configuration

;; Org-mode
(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-agenda-files '("~/clip/mciccale/org")
		org-return-follows-link t
		org-mime-export-options '(:with-toc nil))
  :hook
  (org-mode . my/org-mode-setup)
  (org-mode . my/org-hide-done-entries-in-buffer)
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode))

;; Helper functions
(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 0)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(defun my/org-hide-done-entries-in-buffer ()
  (interactive)
  (org-map-entries #'org-fold-hide-subtree
                   "/+DONE" 'file 'archive 'comment))

;; Additional packages
(use-package org-mime
  :ensure t
  :straight t
  :after org)

(use-package org-bullets
  :ensure t
  :straight t
  :after org
  :hook
  (org-mode . org-bullets-mode))

;; Provide ourselves
(provide 'org-config)

;;; org-config.el ends here
