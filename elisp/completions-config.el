;;; `completions-config.el' --- Configuration related to completions  -*- lexical-binding: t; -*-

(use-package which-key
  :ensure t
  :straight t
  :defer t
  :hook
  (after-init . which-key-mode))

(use-package swiper
  :ensure t
  :straight t)

(use-package ivy
  :ensure t
  :straight t
  :diminish
  :bind
  (("C-s" . swiper-isearch)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   :map ivy-switch-buffer-map
   ("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
   ("C-d" . ivy-reverse-i-search-kill)))
(require 'ivy)
(ivy-mode 1)

(use-package orderless
  :ensure t
  :straight t
  :defer t
  :after ivy
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :straight t
  :hook
  (after-init . marginalia-mode))

(use-package corfu
  :ensure t
  :straight t
  :defer t
  :hook
  (prog-mode . corfu-mode)
  :custom
  (corfu-enable-in-minibuffer nil)
  (corfu-auto nil)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-max-width 50)
  (corfu-min-width 50)
  (corfu-popupinfo-delay 0.5)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :init
  (corfu-popupinfo-mode t))

(use-package nerd-icons-corfu
  :ensure t
  :straight t
  :defer t
  :after (:all corfu))

;; Provide ourselves
(provide 'completions-config)

;;; `completions-config.el' ends here
