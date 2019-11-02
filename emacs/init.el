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
    ("cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" default)))
 '(package-selected-packages
   (quote
    (cider clojure-mode groovy-mode git-link yaml-mode go-mode typescript-mode add-node-modules-path yasnippet company ace-window eyebrowse org-brain company-lsp js2-refactor lsp-mode prettier-js flycheck zenburn-theme zenburn evil-nerd-commenter evil-magit helm-rg dimmer which-key helm-projectile projectile diminish rjsx-mode js2-mode helm evil-smartparens evil-surround evil-escape evil evil-mode magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/config")
;; TODO: Figure out why flycheck says this isn't valid; could have
;;       something to do with it being a symlink
(require 'cv-settings)
(require 'cv-functions)
(require 'cv-packages)
(require 'cv-experiments)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
