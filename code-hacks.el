;; Some functions that are useful when dealing with code.
;; ===========================================================

;; Use instead of add-hook to run it a single time.
;; found on https://emacs.stackexchange.com/questions/3323/is-there-any-way-to-run-a-hook-function-only-once
(defmacro add-hook-run-once (hook function &optional append local)
  "Like add-hook, but remove the hook after it is called"
  (let ((sym (make-symbol "#once")))
    `(progn
       (defun ,sym ()
         (remove-hook ,hook ',sym ,local)
         (funcall ,function))
       (add-hook ,hook ',sym ,append ,local))))

; Help cleanup the includes and using stuff
; found on http://www.emacswiki.org/emacs/DuplicateLines
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun fleury/sort-and-uniquify-region ()
  "Remove duplicates and sort lines in region."
  (interactive)
  (sort-lines nil (region-beginning) (region-end))
  (uniquify-region-lines (region-beginning) (region-end)))

;; Temporarily maximisze a buffer
;; https://gist.github.com/mads379/3402786
(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
