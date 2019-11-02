;;; cv-packages.el --- Set up all the packages

;;; Commentary:
;;; This is where the meat of the configuration comes from.

;;; Code:

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
  (define-key evil-normal-state-map (kbd "SPC b d") 'kill-this-buffer)
  (define-key evil-normal-state-map (kbd "SPC w") evil-window-map)
  (which-key-add-key-based-replacements "SPC w" "window")
  (define-key evil-normal-state-map (kbd "SPC w /") 'evil-window-vsplit) ;; Necessary?
  (define-key evil-normal-state-map (kbd "SPC w -") 'evil-window-split) ;; Necessary?
  (define-key evil-normal-state-map (kbd "SPC w d") 'toggle-window-dedicated)
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
  (define-key evil-normal-state-map (kbd "SPC /") 'helm-projectile-rg)
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
  (define-key evil-motion-state-map (kbd "SPC s") 'ace-swap-window)
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

(use-package cider
  :ensure t)

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
  (add-hook 'js2-mode-hook #'hs-minor-mode)
  (evil-define-key 'normal 'js2-mode-map (kbd "SPC l") 'log-this)
  )

(use-package rjsx-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package groovy-mode
  :ensure t
  :init
  (setq groovy-indent-offset 2))

(use-package clojure-mode
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
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
  :init
  ;; Can't use :hook here because that'll run `lsp-mode`, not `lsp`
  (dolist (hook
	   ;; JavaScript / TypeScript
	   '(js2-mode-hook
	     rjsx-mode-hook
	     typescript-mode-hook
	     ;; Clojure
	     ;; https://github.com/snoe/clojure-lsp#installation
	     clojure-mode-hook
	     clojurec-mode-hook
	     clojurescript-mode-hook))
    (add-hook hook #'lsp))
  (add-hook 'lsp-mode-hook #'(lambda ()
			       (evil-define-key 'normal 'lsp-mode-map (kbd "SPC r r") 'lsp-rename)
			       (evil-define-key 'normal 'lsp-mode-map (kbd "SPC g d") 'lsp-find-implementation)
			       (evil-define-key 'normal 'lsp-mode-map (kbd "SPC g r") 'lsp-find-references))))

(provide 'cv-packages)
;;; cv-packages.el ends here
