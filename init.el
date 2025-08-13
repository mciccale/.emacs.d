;;; `init.el' --- Entrypoint of Emacs config

;; Update load-path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Load the config file
(require 'config)

;; Provide ourselves
(provide 'init)

;;; `init.el' ends here
