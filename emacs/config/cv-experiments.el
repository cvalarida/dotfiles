;;; cv-experiments.el --- A place to test things out

;;; Commentary:
;;;
;;; The idea here is to toy around with functions and settings in
;;; here, then move them to where they belong.
;;;
;;; Probably useful packages:
;;; powerline
;;;
;;; Things I want to do:
;;; Electric brackets
;;; Paste with proper indentation
;;; Show how many matches there are to a search pattern
;;; daemon mode
;;; Hide helm buffers from switch-to-buffer, previous-buffer, and next-buffer
;;;
;;; Things to look into later
;;; evil-leader
;;; Frontmacs config

;;; Code:

(defun rotate-args ()
  "Rotate the highlighted arguments."
  (interactive)
  (let ((original-text (region-text))))
  ;; Split up the text into a list of arguments
  ;; Take the first item in the list, and put it in the back
  ;; Replace the region with the new argument order
  ;; Phase two: Handle multi-line regions by keeping track of the position of the beginning of each argument
  ;;  This could get tricky when handling multiple lines vs single line
  )


;; Org mode note taking
(defun cv/go-to-organizer ()
  "Jump to the organizer org file."
  (interactive) (find-file "~/organizer.org"))
(global-set-key (kbd "C-c o") 'cv/go-to-organizer)
(define-key evil-normal-state-map (kbd "SPC b o")  'cv/go-to-organizer)

;; Set org-refile to show headings from all agenda files
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

;; Posframe
(use-package posframe
  :ensure t
  :config
  (defun my-posframe-arghandler (buffer-or-name arg-name value)
    (let ((info '(
  		  :internal-border-width 3
  		  :internal-border-color "black"
  		  :left-fringe 4
  		  :right-fringe 4)))
      (or (plist-get info arg-name) value)))
  (setq posframe-arghandler #'my-posframe-arghandler)
  )

;; Evil owl
(use-package evil-owl
  :ensure t
  :diminish
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))

;; Use a new frame for Helm buffers
(setq helm-display-function 'helm-display-buffer-in-own-frame
        helm-display-buffer-reuse-frame t
        helm-use-undecorated-frame-option t)

;; This is more hassle than it's worth until I can figure out how to
;; override the org-meta<direction> in evil-org-mode-map.
;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook
;;             (lambda ()
;;               (evil-org-set-key-theme)))
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys)
;;   (define-key evil-org-mode-map (kbd "M-l") 'evil-window-right)
;;   (define-key evil-org-mode-map (kbd "M-k") 'evil-window-up)
;;   (define-key evil-org-mode-map (kbd "M-j") 'evil-window-down)
;;   (define-key evil-org-mode-map (kbd "M-h") 'evil-window-left))

(use-package string-inflection
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "SPC i i")  'string-inflection-cycle)
  (define-key evil-visual-state-map (kbd "SPC i i")  'string-inflection-cycle)
  (define-key evil-normal-state-map (kbd "SPC i c")  'string-inflection-lower-camelcase)
  (define-key evil-visual-state-map (kbd "SPC i c")  'string-inflection-lower-camelcase)
  (define-key evil-normal-state-map (kbd "SPC i s")  'string-inflection-underscore)
  (define-key evil-visual-state-map (kbd "SPC i s")  'string-inflection-underscore)
  )

(provide 'cv-experiments)
;;; cv-experiments.el ends here
