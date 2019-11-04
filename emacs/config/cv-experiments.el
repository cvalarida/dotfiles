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


(provide 'cv-experiments)
;;; cv-experiments.el ends here
