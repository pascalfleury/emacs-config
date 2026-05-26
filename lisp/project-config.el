;;; project-config.el --- Drop-in replacement for projectile-type config functions in project.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pascal Fleury

;; Author: Pascal Fleury <fleury@users.sourceforge.net>
;; Maintainer: Pascal Fleury <fleury@users.sourceforge.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5"))
;; Keywords: project, tools
;; URL: https://github.com

;; This file is NOT part of GNU Emacs.

;;; License:
;; GPL-3.0-or-later

;;; Commentary:
;;
;; This provides a few functions to be drop-in replacements for projectile
;; ones. So the configuration is easier.

;;; Code:


;; Function to implement a drop-in replacement if projectile's
;; projectile-register-project-type and projectile-project-search-path
;;;###autoload
(defcustom project-search-paths '()
  "Directories to automatically scan for project roots."
  :type '(repeat directory)
  :group 'project
  :set (lambda (symbol value)
         ;; Set the standard value first
         (set-default symbol value)
         ;; Run the indexing side-effect loop immediately
         (with-eval-after-load 'project
           (dolist (dir value)
             (when (file-directory-p dir)
               (project-remember-projects-under dir t))))))


;; Drop-in replacement pf projectile-register-project-type
;;;###autoload
(defmacro project-register-project-type (name markers &rest args)
  "Drop-in Projectile API replacement that configures project.el.
NAME is the project type symbol.
MARKERS is a list of file/directory names indicating the project root.
ARGS handles keyword arguments like :compile, :run, and :test."
  (let* ((compile-cmd (plist-get args :compile))
         (run-cmd     (plist-get args :run))
         (test-cmd    (plist-get args :test))
         ;; Derive function and command names dynamically based on NAME symbol
         (try-func    (intern (format "project--try-%s" name)))
         (setup-func  (intern (format "project--setup-%s-settings" name)))
         (run-func    (intern (format "project--%s-run" name)))
         (test-func   (intern (format "project--%s-test" name))))
    `(progn
       ;; 1. Root detection hook & method
       (defun ,try-func (dir)
         (when-let ((root (cl-some (lambda (m) (locate-dominating-file dir m)) ',markers)))
           (cons ',name root)))
       (add-hook 'project-find-functions #',try-func)

       (cl-defmethod project-root ((project (head ,name)))
         (cdr project))

       ;; 2. Buffer-local standard compile command setup
       ,@(when compile-cmd
           `((defun ,setup-func ()
               (when-let ((proj (project-current)))
                 (when (eq (car proj) ',name)
                   (setq-local compile-command ,compile-cmd))))
             (add-hook 'find-file-hook #',setup-func)))

       ;; 3. Dynamic custom interactive commands for Run
       ,@(when run-cmd
           `((defun ,run-func ()
               (interactive)
               (let ((proj (project-current t)))
                 (if (eq (car proj) ',name)
                     (let ((default-directory (project-root proj)))
                       (compile ,run-cmd))
                   (message "This is not a %s project!" ',name))))))

       ;; 4. Dynamic custom interactive commands for Test
       ,@(when test-cmd
           `((defun ,test-func ()
               (interactive)
               (let ((proj (project-current t)))
                 (if (eq (car proj) ',name)
                     (let ((default-directory (project-root proj)))
                       (compile ,test-cmd))
                   (message "This is not a %s project!" ',name))))))

       ;; 5. Keybindings injection inside project-prefix-map
       (with-eval-after-load 'project
         ,@(when run-cmd  `((define-key project-prefix-map "R" #',run-func)))
         ,@(when test-cmd `((define-key project-prefix-map "T" #',test-func)))))))


(provide 'project-config)
;;; project-config.el ends here
