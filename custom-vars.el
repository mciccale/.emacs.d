;;; `custom-vars.el' --- Customize variables file  -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-modes
   '(tex-mode plain-tex-mode texinfo-mode latex-mode doctex-mode))
 '(custom-safe-themes
   '("ed2efc874021b54144b9ed9874e4d9c036090488aeabe19c9ac28576a162ec12"
	 "2c7dc80264de0ba9409d4ebb3c7b31cf8e4982015066174c786f16a672db71b2"
	 "677e3d3d9ca354bfa2422287bc8ef048a759cd5009493e75197d3a3ab3590c58"
	 "546f3e8c4cb46043df1f646322c4b57049fc4c31fdf96e41db077c3408660057"
	 default))
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
