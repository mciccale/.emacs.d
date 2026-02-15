;;; `early-init.el' --- Configurations before init.el  -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated-round . t)))

;;; `early-init.el' ends here
