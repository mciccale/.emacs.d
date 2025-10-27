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
 '(ciao-face-builtin-directive ((t (:inherit font-lock-keyword-face))))
 '(ciao-face-clauseheadname ((t (:inherit font-lock-function-name-face))))
 '(ciao-face-comment ((t (:inherit font-lock-comment-face))))
 '(ciao-face-condcode-directive ((t (:inherit font-lock-keyword-face))))
 '(ciao-face-cut ((t (:inherit font-lock-keyword-face))))
 '(ciao-face-funexp-atom ((t (:inherit font-lock-constant-face))))
 '(ciao-face-library-directive ((t (:inherit font-lock-keyword-face))))
 '(ciao-face-lpdoc-bug-comment ((t (:foreground "Red" :slant italic :weight bold))))
 '(ciao-face-lpdoc-command ((t (:inherit font-lock-constant-face))))
 '(ciao-face-lpdoc-comment ((t (:inherit font-lock-doc-face))))
 '(ciao-face-lpdoc-verbatim ((t (:inherit font-lock-constant-face))))
 '(ciao-face-predicate-directive ((t (:inherit font-lock-constant-face))))
 '(ciao-face-prompt ((t (:inherit font-lock-keyword-face))))
 '(ciao-face-prop-assrt ((t (:inherit font-lock-keyword-face))))
 '(ciao-face-sectioning-5-face ((t (:inherit (variable-pitch font-lock-constant-face) :weight bold))))
 '(ciao-face-string ((t (:inherit font-lock-string-face))))
 '(ciao-face-type-assrt ((t (:inherit font-lock-keyword-face))))
 '(ciao-face-variable ((t (:inherit font-lock-variable-name-face))))
 '(variable-pitch ((t (:height 1.2 :family "TeX Gyre Pagella"))))
 '(variable-pitch-text ((t (:inherit variable-pitch)))))

;; Provide ourselves
(provide 'custom-vars)

;;; `custom-vars.el' ends here
