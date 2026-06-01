;;; show-related-files.el --- Show related C++ files side by side -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pascal Fleury

;; Author: Pascal Fleury <fleury@google.com>
;; Keywords: c++, tools
;; Version: 0.1

;;; Commentary:

;; This package provides a command to show related C++ files (.h, .cc, _test.cc)
;; in side-by-side windows.
;;
;; You can bind this function to a key, for example:
;; (global-set-key (kbd "C-c r") 'paf/show-cc-related-files)
;;
;; You can configure the function used to rotate among files. For example,
;; to use a hypothetical `find-related-files` function:
;;
;; (setq paf/rotate-function 'find-related-files)
;;
;; Or, if you use `use-package`, you can do all this in one statement:
;;
;; (use-package show-related-files
;;   :load-path "/usr/local/google/home/fleury/Emacs/lisp/"
;;   :bind ("C-c r" . paf/show-cc-related-files)
;;   :custom (paf/rotate-function 'find-related-files))

;;; Code:

(require 'windmove)
(require 'rotate-among-files)
(require 'cl-lib)

(defcustom paf/max-rotate-iterations 5
  "Maximum number of times to call `paf/rotate-function` in a loop."
  :type 'integer
  :group 'paf)

(defcustom paf/rotate-function 'google-rotate-among-files
  "Function to call to rotate among related files."
  :type 'function
  :group 'paf)

(defun paf/is-header-file-p (filename)
  "Return non-nil if FILENAME is a C++ header file."
  (and filename (string-suffix-p ".h" filename)))

(defun paf/is-cc-file-p (filename)
  "Return non-nil if FILENAME is a C++ source file."
  (and filename (string-suffix-p ".cc" filename)))

(defun paf/is-test-file-p (filename)
  "Return non-nil if FILENAME is a C++ test file."
  (and filename (string-suffix-p "_test.cc" filename)))

(defun paf/get-window-to-right ()
  "Return the window to the right, splitting if necessary."
  (let ((w-right (window-in-direction 'right)))
    (if w-right
        (select-window w-right)
      (progn
        (split-window-right)
        (select-window (window-in-direction 'right))))))


(defun paf/get-file-slot (filename)
  "Return the slot index for FILENAME based on predicates.
Returns nil if no predicate matches."
  (cond
   ((paf/is-header-file-p filename) 0)
   ((paf/is-test-file-p filename) 2)
   ((paf/is-cc-file-p filename) 1)
   (t nil)))


(defun paf/get-related-files-list ()
  "Return a list of related files, starting with the current file.
Uses `paf/rotate-function` to find related files and slots them by type."
  (let ((initial-file (buffer-file-name))
        (slots (make-vector 3 nil))
        (others nil))
    (unless initial-file
      (user-error "Current buffer is not visiting a file"))

    ;; Process initial file
    (let ((slot (paf/get-file-slot initial-file)))
      (if slot
          (aset slots slot initial-file)
        (push initial-file others)))

    ;; Collect and slot files until cycle or limit
    (catch 'cycled
      (dotimes (i paf/max-rotate-iterations)
        (funcall paf/rotate-function)
        (let ((current-file (buffer-file-name)))
          (if (or (not current-file) (string-equal current-file initial-file))
              (throw 'cycled nil)
            (let ((slot (paf/get-file-slot current-file)))
              (if slot
                  (aset slots slot current-file)
                (push current-file others)))))))

    ;; Restore to initial file
    (find-file initial-file)

    ;; Clean nil from slots and append others
    (let ((result nil))
      (dotimes (i 3)
        (let ((val (aref slots i)))
          (when val
            (push val result))))
      (setq result (nreverse result))
      (append result (nreverse others)))))

(defun paf/show-cc-related-files ()
  "Show related files in side-by-side windows."
  (interactive)
  (let ((files (paf/get-related-files-list))
        (original-window (selected-window)))

    (dolist (file (cdr files))
      (paf/get-window-to-right)
      (find-file file))

    (select-window original-window)))


(provide 'show-related-files)
;;; show-related-files.el ends here
