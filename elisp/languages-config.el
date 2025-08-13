;;; `languages-config.el' --- (Programming) languages configuration

;; Ciao
(if (file-exists-p "~/clip/Systems/ciao-devel/bndls/ciao_emacs/elisp/ciao-site-file.el")
  (load-file "~/clip/Systems/ciao-devel/bndls/ciao_emacs/elisp/ciao-site-file.el"))

(add-hook 'ciao-mode-hook #'display-line-numbers-mode)

(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :init
  (setq treesit-language-source-alist '((json "https://github.com/tree-sitter/tree-sitter-json")))
  :config
  (setq treesit-auto-langs '(json toml yaml))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;; LSP
(use-package lsp-mode
  :ensure t
  :straight t
  :defer t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ((lean4-mode) . lsp-deferred))
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-inlay-hint-enable t)
  (lsp-completion-provider :none)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-idle-delay 0.500)
  (lsp-keep-workspace-alive nil)
  ;; Core settings
  (lsp-enable-xref t)
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding nil)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation nil)                          ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color t)                    ;; Enable text document color.
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)                ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable nil)                 ;; Use `flymake' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
  (lsp-eldoc-render-all t)                              ;; Render all ElDoc messages.
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet nil)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Lens settings
  (lsp-lens-enable t)                                   ;; Enable lens support.
  ;; Headerline settings
  (lsp-headerline-breadcrumb-enable nil)   ;; Enable symbol numbers in the headerline.
  ;; Semantic settings
  (lsp-semantic-tokens-enable t))                     ;; Disable semantic tokens.

;; Lean4
(use-package lean4-mode
  :ensure t
  :straight (lean4-mode :type git :host github
                        :repo "leanprover-community/lean4-mode"
                        :files ("*.el" "data"))
  :commands lean4-mode)

;; Auto-match parentheses
(use-package smartparens
  :ensure t
  :straight t
  :hook (prog-mode text-mode markdown-mode ciao-mode ciao-inferior-mode)
  :config (require 'smartparens-config))

;; Snippets
(use-package yasnippet
  :ensure t
  :straight t
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
                           (setq display-line-numbers-mode nil)
						   (if (eq system-type 'darwin)
							   (setq pdf-view-use-scaling t)
							 (setq pdf-view-use-scaling nil)))))

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
