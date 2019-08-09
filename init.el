;;; init.el --- An artisanal, hand-rolled emacs configuration.

;;; Commentary:
;;; Pretty much what you expect.  My plan is to break this out into separate files later.

;;; Code:

;;;;;;;;;;;;;;;;;;;
;;               ;;
;;   Use MELPA   ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" default)))
 '(package-selected-packages
   (quote
    (groovy-mode git-link yaml-mode go-mode typescript-mode add-node-modules-path yasnippet company ace-window eyebrowse org-brain company-lsp js2-refactor lsp-mode prettier-js flycheck zenburn-theme zenburn evil-nerd-commenter evil-magit helm-rg dimmer which-key helm-projectile projectile diminish rjsx-mode js2-mode helm evil-smartparens evil-surround evil-escape evil evil-mode magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; Define some custom functions
;; https://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun yank-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Make the mode line less obnoxious
;; NOTE: Diminishing should be happening in the use-package calls where possible
(use-package diminish
  :ensure t
  :config
  (add-hook 'undo-tree-mode-hook #'(lambda () (diminish 'undo-tree-mode)))
  (add-hook 'auto-revert-mode-hook #'(lambda () (diminish 'auto-revert-mode)))
  (add-hook 'flymake-mode-hook #'(lambda () (diminish 'flymake-mode)))
  (add-hook 'eldoc-mode-hook #'(lambda () (diminish 'eldoc-mode)))
  (add-hook 'yas-minor-mode-hook #'(lambda () (diminish 'yas-minor-mode)))
  (add-hook 'hs-minor-mode-hook #'(lambda () (diminish 'hs-minor-mode))))

;; Install some packages!
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package evil
  :ensure t
  :bind
  ("M-h" . evil-window-left)
  ("M-j" . evil-window-down)
  ("M-k" . evil-window-up)
  ("M-l" . evil-window-right)
  :init
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-symbol-word-search t) ;; Search (*#) for symbols, not words
  :config
  (define-key evil-normal-state-map (kbd "g D") 'xref-find-definitions-other-window)
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (which-key-add-key-based-replacements "SPC b" "buffers")
  (define-key evil-normal-state-map (kbd "SPC b n") 'next-buffer)
  (define-key evil-normal-state-map (kbd "SPC b p") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "SPC b d") 'evil-delete-buffer)
  (define-key evil-normal-state-map (kbd "SPC w") evil-window-map)
  (which-key-add-key-based-replacements "SPC w" "window")
  (define-key evil-normal-state-map (kbd "SPC w /") 'evil-window-vsplit)
  (define-key evil-normal-state-map (kbd "SPC w -") 'evil-window-split)
  (which-key-add-key-based-replacements "SCP f" "files")
  (define-key evil-normal-state-map (kbd "SPC f f") 'helm-find-files)
  (define-key evil-normal-state-map (kbd "SPC f y") 'yank-buffer-file-name)
  (evil-mode))

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t
  :config
  (which-key-add-key-based-replacements "SPC g" "git")
  ;; Not sure why I couldn't do this in magit's :config
  (define-key evil-normal-state-map (kbd "SPC g s") 'magit)
  (define-key evil-normal-state-map (kbd "SPC g b") 'magit-blame))

(use-package git-link
  :ensure t
  :config
  (setq git-link-default-branch "master"))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x))
  :config
  (define-key evil-normal-state-map (kbd "SPC b b") 'helm-mini)
  (helm-mode))

(use-package helm-rg
  :ensure t)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

(use-package evil-smartparens
 :ensure t
 :diminish evil-smartparens-mode
 :init
 ;; It doesn't like using :hook
 (add-hook 'smartparens-mode-hook 'evil-smartparens-mode))

(use-package projectile
  :ensure t)

(use-package helm-projectile
  :ensure t
  ;; :bind (("SPC p" . helm-projectile))
  :config
  (which-key-add-key-based-replacements "SPC p" "project")
  (define-key evil-normal-state-map (kbd "SPC p f") 'helm-projectile)
  (define-key evil-normal-state-map (kbd "SPC p /") 'helm-projectile-rg)
  (helm-projectile-on))

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (define-key evil-motion-state-map (kbd "SPC ;") 'evilnc-comment-operator))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  (which-key-add-key-based-replacements "SPC e" "flycheck")
  (define-key evil-normal-state-map (kbd "SPC e") flycheck-command-map)
  )

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook
	    #'(lambda ()
		;; Move company-files to the front of the list to get auto-completion working for file paths in JS files
		(setq company-backends (cons 'company-files (delq 'company-files company-backends)))
		)))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package org-brain
  :ensure t
  :init
  (setq org-brain-path "~/.emacs.d/org-brain-files/")
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs)))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-setup-evil-keys)
  (eyebrowse-mode))

(use-package winner
  :ensure t
  :config
  ;; Return to previous window configuration ("hacky")
  (winner-mode)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute
   'aw-leading-char-face nil :height 3.0)
  (define-key evil-motion-state-map (kbd "SPC TAB") 'ace-window)
  ;; ace-window requires avy, so set up avy keys here
  (define-key evil-motion-state-map (kbd "SPC j j") 'evil-avy-goto-char-timer))

(use-package yasnippet
  ;; yasnippet is installed as a dependency of another package, but
  ;; make sure it's installed anyhow and configure it
  :ensure t
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "M-/") yas-maybe-expand)
  (yas-global-mode 1))

;; Major modes
(use-package js2-mode
  :ensure t
  :init
  ;; (setq js-basic-indent 2)
  (setq-default js-indent-level 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON"))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook #'hs-minor-mode))

(use-package rjsx-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

;; Packages that add hooks to major modes
(use-package add-node-modules-path
  :ensure t
  :init
  (dolist (hook '(js2-mode-hook rjsx-mode-hook))
    (add-hook hook #'add-node-modules-path)))

(use-package prettier-js
  :ensure t
  :diminish prettier-js-mode
  :init
  (dolist (hook '(js2-mode-hook rjsx-mode-hook))
    (add-hook hook 'prettier-js-mode)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :diminish lsp-mode "LSP"
  :init
  ;; Can't use :hook here because that'll run `lsp-mode`, not `lsp`
  (dolist (hook '(js2-mode-hook rjsx-mode-hook typescript-mode-hook))
    (add-hook hook #'lsp))
  (add-hook 'lsp-mode-hook #'(lambda ()
			       (define-key evil-normal-state-map (kbd "SPC g d") 'lsp-find-implementation)
			       (define-key evil-normal-state-map (kbd "SPC g r") 'lsp-find-references))))

(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode
  :config
  (dolist (hook '(js2-mode-hook rjsx-mode-hook))
    (add-hook hook #'js2-refactor-mode))
  (which-key-add-key-based-replacements "SPC r" "refactor")
  ;; Turns out this map is completely empty
  ;; TODO: Fill it out
  (define-key evil-normal-state-map (kbd "SPC r") js2-refactor-mode-map))

;; Probably useful packages:
;; powerline

;; Things I want to do:
;; Show directory contents when typing out paths (import something from '../../')
;;   Of course this works when I type out the path from this file...:confused:
;; Electric brackets
;; Paste with proper indentation
;; More sensible default window layouts
;;   (done...somehow?)
;; Open *special* buffers down at the bottom (or at least some)
;;   (Waiting to see if I'm okay with the way it is now)
;; Show how many matches there are to a search pattern
;; daemon mode
;; Hide helm buffers from switch-to-buffer, previous-buffer, and next-buffer

;; Things to look into later
;; evil-leader
;; Look at Frontmacs config

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
