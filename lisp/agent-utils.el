;;; agent-utils.el --- Copy Agetn-compliant file references -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides functions to extract file and region information
;; and format them as references suitable for agent prompts.
;;
;; Example keybinding:
;; (global-set-key (kbd "C-P R") 'paf/file-reference-copy-reference)

;;; Code:

(require 'project)


(defun paf/file-reference-get-region-info ()
  "Extract file path, start and end line numbers of the current region.

Returns a list: (FILENAME START-LINE END-LINE).
Signals a `user-error` if not visiting a file or no region is active."
  (unless (and (buffer-file-name) (use-region-p))
    (user-error "Buffer is not visiting a file or no region is active"))

  (let* ((filename (buffer-file-name))
         (start-line (line-number-at-pos (region-beginning)))
         (end-line (line-number-at-pos (region-end))))
    (list filename start-line end-line)))


(defun paf/file-reference-format-reference (filename start-line end-line)
  "Format the file info into an agent reference string and add to kill ring.

FILENAME is the path to the file.
START-LINE and END-LINE define the range."
  (let ((reference (format "@%s:%d-%d" filename start-line end-line)))
    (kill-new reference)
    (message "Copied to kill ring: %s" reference)))


;;;###autoload
(defun paf/file-reference-copy ()
  "Main command to copy a file reference for the current region.

This function uses `project.el` logic where applicable and formats the
path for the kill ring."
  (interactive)
  (let ((info (paf/file-reference-get-region-info)))
    (when info
      (apply #'paf/file-reference-format-reference info))))


(provide 'agent-utils)

;;; agent-utils.el ends here
