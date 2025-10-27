;;; org-config.el --- Org-mode configuration

;; Org-mode
(use-package org
  :ensure nil
  :defer t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :init
  ;; General settings
  (setq org-directory "~/clip/Theses/MarcoCiccale_PhD/org"
		org-agenda-files '("inbox.org" "agenda.org")
		org-return-follows-link t
		org-mime-export-options '(:with-toc nil))

  ;; Agenda
  (setq org-agenda-hide-tags-regexp "."
		org-agenda-prefix-format    '((agenda . " %i %-12:c%?-12t% s")
									  (todo   . " %i %-12:c")
									  (tags   . " %i %-12:c")
									  (search . " %i %-12:c")))
  :hook
  (org-mode . my/org-mode-setup)
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  (org-capture-mode . delete-other-windows))

;; Helper functions
(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 0)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

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


;; Org Capture-mode

;; ;; Helper functions
;; (defun my/org-capture-add-created-property ()
;;   "Add CREATED property to the current capture entry."
;;   (org-set-property "CREATED" (format-time-string "<%Y-%m-%d %H:%M>")))

;; Templates
(setq org-capture-templates
	  '(;; Random thoughts that come to mind
		("i" "Inbox" entry (file "inbox.org")
		 "* TODO %?\n")

		;; "Reply to" entries
		("@" "Inbox [mu4e]" entry (file "inbox.org")
		 "* TODO Check \"%a\" %?\n")

		;; Meetings/appointments
		("m" "Meeting" entry (file+headline "agenda.org" "Future")
		 "* %? :meeting:%^{Where?}:\nSCHEDULED: <%<%Y-%m-%d %a %H:00>>")

		;; Notes for meeting
		("n" "Note" entry (file "notes.org")
		 "* Note (%a)\n\n%?")))

;; (add-hook 'org-capture-before-finalize-hook 'my/org-capture-add-created-property)

;; Provide ourselves
(provide 'org-config)

;;; org-config.el ends here
