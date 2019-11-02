;;; cv-functions.el --- Define some custom functions

;;; Commentary:

;;; Code:

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

;; Toggle window dedication
;; From https://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window
				 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun log-this ()
  "Insert a console.log() for the symbol under the point.

TODO: Make this useful for more than just JavaScript."
  (interactive)
  (let ((thing-to-log
	 (if (use-region-p)
	     (buffer-substring-no-properties (region-beginning) (region-end))
	   (thing-at-point 'word))))
    (save-excursion
      (end-of-line)
      (insert (if use-hard-newlines hard-newline "\n"))
      (indent-according-to-mode)
      (insert (concat "console.log('" thing-to-log ":', " thing-to-log ");"))
    )))

(provide 'cv-functions)
;;; cv-functions.el ends here
