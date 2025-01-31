;;; paf-functions --- collection of random functions

;;; Commentary:

;;; Code:

;; Function to run a hook only once
;; Found here: https://emacs.stackexchange.com/questions/3323/is-there-any-way-to-run-a-hook-function-only-once
(defmacro add-hook-run-once (hook function &optional append local)
  "Like add-hook, but remove the hook after it is called"
  (let ((sym (make-symbol "#once")))
    `(progn
       (defun ,sym ()
         (remove-hook ,hook ',sym ,local)
         (funcall ,function))
       (add-hook ,hook ',sym ,append ,local))))

(defun paf/truncate-string (text &optional len ellipsis)
    "Truncate the text to a given length.

  When LEN is a number, resulting string is truncated at that length.
  If the length is bigger, then '...' is added at the end.

  Usage example:

    (setq org-agenda-prefix-format
          '((agenda . \" %(paf/truncate-string (roam-extras/extract-agenda-category) 12) %?-12t %12s\")))

  Refer to `org-agenda-prefix-format' for more information."
    (interactive)
    (if (and (numberp len) (> (length text) len))
        (let* ((used-ellipsis (if (eq ellipsis nil) "…" ellipsis))
               (ellipsis-length (length used-ellipsis))
               (short-text (substring text 0 (- len ellipsis-length))))
          (format "%s%s" short-text used-ellipsis))
      text))

  ;; (setq paf-tests/truncate (paf/truncate-string "Here is some long text" 10))


;; It actually computes the entire arithmetic expression that is selected, and replaces it with the numerical result.
(defun apply-function-to-region (fn)
  (interactive "XFunction to apply to region: ")
  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
           (had-region (use-region-p))
           (resulting-text
            (funcall
             fn
             (buffer-substring-no-properties beg end)))
           (new-end (+ beg (length resulting-text))))
      (kill-region beg end)
      (insert resulting-text)
      ;; set the active region again if it was set originally.
      (if had-region
          (progn
            (goto-char beg)
            (push-mark new-end)
            (setq mark-active t))))))

(defun paf/sum-amount (expression)
  "Computes the sum from the arith expression given as argument."
  (format "%.2f" (string-to-number (calc-eval expression))))

(defun paf/sum-amount-of-region ()
  "Takes the region as an arithmetic expr, and replaces it with its sum."
  (interactive)
  (if (use-region-p)
      (progn
        (apply-function-to-region 'paf/sum-amount)
        (goto-char (region-end)))))

;; Adapted from here: http://www.emacswiki.org/emacs/DuplicateLines
(defun paf/uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun paf/sort-and-uniquify-region ()
  "Remove duplicates and sort lines in region."
  (interactive)
  (sort-lines nil (region-beginning) (region-end))
  (paf/uniquify-region-lines (region-beginning) (region-end)))

(provide 'paf-functions)
;;; paf-functions ends here
