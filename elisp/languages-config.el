;;; `languages-config.el' --- (Programming) languages configuration

;; Ciao
(if (file-exists-p "~/clip/Systems/ciao-devel/bndls/ciao_emacs/elisp/ciao-site-file.el")
  (load-file "~/clip/Systems/ciao-devel/bndls/ciao_emacs/elisp/ciao-site-file.el"))

(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;; Eglot

;; Auto-match parentheses
(use-package smartparens
  :hook (prog-mode text-mode markdown-mode ciao-mode ciao-inferior-mode)
  :config (require 'smartparens-config))

;; Snippets
(use-package yasnippet
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

;; Load Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Markdown
(use-package markdown-mode
  :defer t
  :straight t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; PDF-tools
(use-package pdf-tools
  :ensure t
  :straight t
  :custom (pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
              ("C-s"     . isearch-forward)
              ("C-c C-f" . my/slides-presentation))
  :hook (pdf-view-mode . (lambda ()
                           (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
                           (setq display-line-numbers-mode nil)))
  :config (setq pdf-view-use-scaling nil))

;; Install/load PDF-tools
(require 'pdf-tools)
(pdf-tools-install)

;; AUCTeX
(use-package auctex
  :defer t
  :ensure t
  :straight t
  :hook
  (LaTeX-mode . (lambda ()
				  (pdf-tools-install) ;; TODO: Redundant?
				  (outline-minor-mode 1)
				  (display-line-numbers-mode 1)
				  (add-hook 'pdf-view-mode-hook
							(lambda () (display-line-numbers-mode -1))
							:append :local)

				  (setq fill-column 70)
				  (setq TeX-auto-save t
						TeX-parse-self t
						TeX-show-compilation nil
						TeX-global-PDF-mode t
						TeX-clean-confirm nil
						TeX-command-default "LaTeX"
						TeX-view-program-selection '((output-pdf "PDF Tools"))
						TeX-source-correlate-mode t
						TeX-source-correlate-method '((dvi . source-specials)
													  (pdf . synctex))
						TeX-source-correlate-start-server t)

				  (local-set-key (kbd "M-q") #'LaTeX-fill-paragraph))))

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; Provide ourselves
(provide 'languages-config)

;;; `languages-config.el' ends here
