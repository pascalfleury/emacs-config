;;; project-config.el --- Drop-in replacement for projectile-type config functions in project.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pascal Fleury

;; Author: Pascal Fleury <fleury@users.sourceforge.net>
;; Maintainer: Pascal Fleury <fleury@users.sourceforge.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: project, tools
;; URL: https://github.com

;; This file is NOT part of GNU Emacs.

;;; License:
;; GPL-3.0-or-later

;;; Commentary:
;;
;; This provides a few functions to be drop-in replacements for projectile
;; ones. So the configuration is easier.
;; Functions implemented:
;;  - projectile-register-project-type  (defun)
;;  - project-search-paths              (defcustom)
;;
;; Note you really need to use (set-custom-variable 'project-search-path ( "~/Projects" ))
;; for ti to work.

;;; Code:


;;;###autoload
(defcustom project-search-paths '()
  "Directories to automatically scan for project roots.
Updates both Projectile and project.el settings dynamically based on what is loaded."
  :type '(repeat directory)
  :group 'project
  :set (lambda (symbol value)
         ;; 1. Update this variable's local value first
         (set-default symbol value)

         ;; 2. Conditional side-effect logic
         (if (featurep 'projectile)
             ;; If Projectile is running, sync its path variable directly
             (setq projectile-project-search-path value)

           ;; Fallback: If using pure project.el, index the cache immediately
           (with-eval-after-load 'project
             (dolist (dir value)
               (when (file-directory-p dir)
                 ;; The 't' flag tells it to search recursively down one level
                 (project-remember-projects-under dir t)))))))


;; Drop-in replacement pf projectile-register-project-type
;;;###autoload
(defmacro projectile-register-project-type (name markers &rest args)
  "Register a project type natively.
If Projectile is loaded, it forwards to `projectile-register-project-type`.
Otherwise, it implements the behavior natively for `project.el`."
  (if (featurep 'projectile)
      ;; Branch A: Projectile is active. Forward the call directly.
      `(projectile-register-project-type ,name ,markers ,@args)

    ;; Branch B: Projectile is absent. Configure project.el instead.
    (let* ((compile-cmd     (plist-get args :compile))
           (run-cmd         (plist-get args :run))
           (test-cmd        (plist-get args :test))
           (compilation-dir (plist-get args :compilation-dir))
           (test-dir        (plist-get args :test-dir))
           (src-dir         (plist-get args :src-dir))
           ;; Derive configuration names based on the NAME symbol
           (try-func        (intern (format "project-try-%s" name)))
           (setup-func      (intern (format "project-setup-%s-settings" name)))
           (run-func        (intern (format "project-%s-run" name)))
           (test-func       (intern (format "project-%s-test" name))))
      `(progn
         ;; 1. Root detection hook & object properties mapping
         (defun ,try-func (dir)
           (when-let ((root (cl-some (lambda (m) (locate-dominating-file dir m)) ',markers)))
             (list ',name root ',args)))
         (add-hook 'project-find-functions #',try-func)

         (cl-defmethod project-root ((project (head ,name)))
           (cadr project))

         (cl-defmethod project-src-dir ((project (head ,name)))
           (let ((plist (caddr project)))
             (expand-file-name (or (plist-get plist :src-dir) "") (project-root project))))

         ;; 2. Set buffer-local compile configurations & compilation-dir overrides
         ,@(when compile-cmd
             `((defun ,setup-func ()
                 (when-let ((proj (project-current)))
                   (when (eq (car proj) ',name)
                     (let* ((root (project-root proj))
                            (c-dir (expand-file-name (or ,compilation-dir "") root)))
                       (setq-local compile-command ,compile-cmd)
                       (setq-local default-directory c-dir)))))
               (add-hook 'find-file-hook #',setup-func)))

         ;; 3. Interactive command for Run
         ,@(when run-cmd
             `((defun ,run-func ()
                 (interactive)
                 (let ((proj (project-current t)))
                   (if (eq (car proj) ',name)
                       (let ((default-directory (project-root proj)))
                         (compile ,run-cmd))
                     (message "This is not a %s project!" ',name))))))

         ;; 4. Interactive command for Test (Respects test-dir)
         ,@(when test-cmd
             `((defun ,test-func ()
                 (interactive)
                 (let ((proj (project-current t)))
                   (if (eq (car proj) ',name)
                       (let* ((root (project-root proj))
                              (t-dir (expand-file-name (or ,test-dir "") root))
                              (default-directory t-dir))
                         (compile ,test-cmd))
                     (message "This is not a %s project!" ',name))))))

         ;; 5. Keybindings binding into standard project-prefix-map
         (with-eval-after-load 'project
           ,@(when run-cmd  `((define-key project-prefix-map "R" #',run-func)))
           ,@(when test-cmd `((define-key project-prefix-map "T" #',test-func))))))))


(provide 'project-config)
;;; project-config.el ends here
