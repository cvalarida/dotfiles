

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
