;;; `custom-vars.el' --- Customize variables file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-modes
   '(tex-mode plain-tex-mode texinfo-mode latex-mode doctex-mode))
 '(font-latex-fontify-sectioning 1.3))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:height 1.2 :family "TeX Gyre Pagella"))))
 '(variable-pitch-text ((t (:inherit variable-pitch)))))

;; Provide ourselves
(provide 'custom-vars)

;;; `custom-vars.el' ends here
