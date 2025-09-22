;;; config.el --- My Emacs configuration (using `straight.el')

;; Performance hacks
(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))

;; Disable `package.el' by default
(setq package-enable-at-startup nil)

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Packages
(require 'package)
(setq package-archives '(("melpa"          . "https://melpa.org/packages/")
                         ("melpa (stable)" . "https://stable.melpa.org/packages/")
                         ("org"            . "https://orgmode.org/elpa/")
                         ("elpa"           . "https://elpa.gnu.org/packages/")))

;; Core Emacs configuration
(use-package emacs
  :ensure nil
  :custom
  (column-number-mode t)                     ;; Display the column number in the mode line.
  (auto-save-default nil)                    ;; Disable automatic saving of buffers.
  (create-lockfiles nil)                     ;; Prevent the creation of lock files when editing.
  (delete-by-moving-to-trash t)              ;; Move deleted files to the trash
  (display-line-numbers-type 'relative)      ;; Use relative line numbering in programming modes.
  ;; (global-auto-revert-non-file-buffers t)    ;; Automatically refresh non-file buffers.
  (history-length 25)                        ;; Set the length of the command history.
  (inhibit-startup-message t)                ;; Disable the startup message when Emacs launches.
  (initial-scratch-message "")               ;; Clear the initial message in the *scratch* buffer.
  (ispell-dictionary "en_US")                ;; Set the default dictionary for spell checking.
  ;; (pixel-scroll-precision-mode t)            ;; Enable precise pixel scrolling.
  ;; (pixel-scroll-precision-use-momentum nil)  ;; Disable momentum scrolling for pixel precision.
  (ring-bell-function 'ignore)               ;; Disable the audible bell.
  (switch-to-buffer-obey-display-actions t)  ;; Make buffer switching respect display actions.
  (tab-width 4)                              ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                ;; Use advanced font locking for Treesit mode.
  (truncate-lines nil)                       ;; Enable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                       ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                      ;; Use short answers in prompts for quicker responses
  (warning-minimum-level :emergency)         ;; Set the minimum level of warnings to display.
  (add-log-dont-create-changelog-file t)     ;; Don't create changelog files by default
  (sp-highlight-pair-overlay nil)            ;; Don't highlight insides matching parens
  (calendar-date-style 'european)            ;; The *only* right way of formatting dates
  (calendar-week-start-day 1)                ;; Who the hell thinks that weeks start on Sundays???
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (setq system-time-locale "C")
  (set-face-attribute 'default nil :family "JetBrainsMono NF" :height 120)
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (set-face-attribute 'default nil :family "JetBrainsMono NF" :height 150))

  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                       ;; Load the custom file quietly, ignoring errors.

  ;; Vertical divisor (│).
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :init
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (when scroll-bar-mode
    (scroll-bar-mode 0))
  
  (global-hl-line-mode -1)     ;; Disable highlight of the current line
  ;; (global-auto-revert-mode 1)  ;; Enable global auto-revert mode
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.

  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Custom keybinds
  (global-unset-key (kbd "M-%"))
  (global-set-key (kbd "M-%") 'query-replace-regexp)

  ;; Add a hook to run code after Emacs has fully initialized.
  (add-hook 'after-init-hook
            (lambda ()
			  (server-start)
              (message "Emacs has fully loaded. This code runs after startup.")
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format
                         ";; Welcome to Emacs!
;;
;; Loading time : %s

"
                         (emacs-init-time)))))))

;; Centralize backup files under .emacs.d
(setopt make-backup-file-name-function
	(lambda (fpath)
	  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
		 (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
		 (backupFilePath
		  (replace-regexp-in-string
		   "//" "/" (concat backupRootDir filePath "~"))))
	    (make-directory (file-name-directory backupFilePath)
			    (file-name-directory backupFilePath))
	    backupFilePath)))

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1)))))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lahv --group-directories-first")
  (dired-dwim-target t)
  ;; (dired-kill-when-opening-new-dired-buffer t)
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls)))))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0.3)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

(require 'completions-config)
(require 'org-config)
(require 'languages-config)
(require 'evil-config)
(require 'misc-packages-config)
(require 'ui-config)
(require 'my-functions)

(unless (eq system-type 'darwin)
  (require 'mail-config))

;; Load theme
(use-package ef-themes
  :ensure t
  :straight t
  :defer t)

(load-theme 'ef-arbutus :no-confirm)

;; Provide ourselves
(provide 'config)

;;; `config.el' ends here
