;; sensitive-mode.el --- minor mode for sensitive info containing files.

;; Copyright by the original author, code found here:
;; https://anirudhsasikumar.net/blog/2005.01.21.html

;;; Commentary:

;; Defines a minor mode that prevents auto-saving for files that
;; contain sensitive data.

;;;###autoload

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :init-value nil
  :lighter " Sensitive"
  :keymap nil
  :after-hook
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))
