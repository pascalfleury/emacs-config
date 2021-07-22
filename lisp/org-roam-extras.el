;; Some functions to use GTD withint org-roam files without
;; taxing the org-agenda too much.

;; TODO: org-roam has changed, and there are no org-roam--* functions anymore.
;;       need to find otu how to implement this functionality here, or
;;       ditch org-roam.

(defcustom roam-extras-todo-tag-name "todo"
  "The tag to use to mark files containing org-mode todo items."
  :type '(string))

(defcustom roam-extras-org-agenda-files-cache nil
  "Keeps the original org-agenda-files while it tweaks it."
  :type 'sexp)

(defun roam-extras/extract-agenda-category ()
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(roam-extras/extract-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         ;; TODO: this does not extract the title anymore.
         (title) ;; (car-safe (org-roam--extract-titles-title)))
         (category (org-get-category)))
    (or (if (and
             title
             (string-equal category file-name))
            title
          category)
        "")))

(defun roam-extras/todo-p ()
  "Return non-nil if current buffer has any TODO entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))

(defun roam-extras/update-todo-tag ()
  "Update TODO tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (org-roam--org-file-p buffer-file-name))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (all-tags (org-roam--extract-tags file))
           (prop-tags (org-roam--extract-tags-prop file))
           (tags prop-tags))
      (if (roam-extras/todo-p)
          (setq tags (seq-uniq (cons roam-extras-todo-tag-name tags)))
        (setq tags (remove roam-extras-todo-tag-name tags)))
      (unless (equal prop-tags tags)
        (org-roam--set-global-prop
         "filetags"
         (combine-and-quote-strings tags))))))

(defun roam-extras/todo-files ()
  "Return a list of note files containing todo tag."
  (let* ((search-tag (concatenate 'string "%\"" roam-extras-todo-tag-name "\"%")))
    (seq-map
     #'car
     (org-roam-db-query
      [:select file
               :from tags
               :where (like tags (quote search-tag))]))))

(defun roam-extras/update-todo-files (&rest _)
  "Sets the value of `org-agenda-files' to only relevant org-roam files."
  (setq roam-extras-org-agenda-files-cache org-agenda-files)
  (setq org-agenda-files (roam-extras/todo-files)))

(defun roam-extras/add-todo-files (&rest _)
  "Extends the value of `org-agenda-files' with relevant org-roam files."
  (setq roam-extras-org-agenda-files-cache org-agenda-files)
  (setq org-agenda-files (append org-agenda-files (roam-extras/todo-files))))

(defun roam-extras/restore-todo-files (&rest _)
  "Extends the value of `org-agenda-files' with relevant org-roam files."
  (setq org-agenda-files roam-extras-org-agenda-files-cache))

(provide 'org-roam-extras)
