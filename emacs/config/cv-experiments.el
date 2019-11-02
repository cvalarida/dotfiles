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

;; TODO:
;; - Log a region
(defun log-this ()
  "Insert a console.log() for the symbol under the point."
  (interactive)
  (let ((thing-to-log (thing-at-point 'word)))
    (save-excursion
      (end-of-line)
      (insert (if use-hard-newlines hard-newline "\n"))
      (indent-according-to-mode)
      (insert (concat "console.log('" thing-to-log ":', " thing-to-log ");"))
    )))

(provide 'cv-experiments)
;;; cv-experiments.el ends here
