;;; cv-settings.el --- Setting some sane defaults

;;; Commentary:
;;; Any settings outside of packages should be set up here.

;;; Code:

;; Set up some defaults
(show-paren-mode)
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq scroll-conservatively 2)

;; Get rid of the visual clutter
(toggle-frame-fullscreen)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Global key bindings
(global-set-key (kbd "<M-left>") 'previous-buffer)
(global-set-key (kbd "<M-right>") 'next-buffer)

;; Set up key bindings for mode maps that aren't a part of a package.
(defun set-up-org-mode-map ()
  "Set up org mode key maps."
  (define-key org-mode-map (kbd "M-h") nil)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link))
(add-hook 'org-mode-hook #'set-up-org-mode-map)

;; Stop backup-files~ from getting littered everywhere
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.backup-files/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Set shell indentation
(setq sh-indentation 2)

;; Configure ediff
;; From https://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  "Variant of 'setq', but aware of the 'custom-set' property of VARIABLE.  Set VARIABLE to VALUE."
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
;; Keep instructions in the same frame
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Split horizontally to see changes more easily
(csetq ediff-split-window-function 'split-window-horizontally)
;; Ignore whitespace
;; (csetq ediff-diff-options "-w")
;; Keybindings
(defun ediff-setup-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))
(add-hook 'ediff-mode-hook 'ediff-setup-hook)


(provide 'cv-settings)
;;; cv-settings.el ends here
