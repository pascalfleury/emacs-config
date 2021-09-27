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

;; Layer to get the roam tags as lists. Note that Magnus uses space
;; but #+filetags: are ':' separated actually.
(defun roam-extras/get-filetags ()
  (split-string (or (org-roam-get-keyword "filetags") "") "[ :]+" t))

(defun roam-extras/set-filetags (tags)
  (let* ((tags-str (combine-and-quote-strings tags ":"))
         (filetags (if (= (length tags-str) 0)
                     ""
                     (concat ":" tags-str ":"))))
    (org-roam-set-keyword "filetags" filetags)))

(defun roam-extras/add-filetag (tag)
  (let* ((new-tags (cons tag (roam-extras/get-filetags))))
    (roam-extras/set-filetags new-tags)))

(defun roam-extras/del-filetag (tag)
  (let* ((new-tags (seq-difference (roam-extras/get-filetags) `(,tag))))
    (roam-extras/set-filetags new-tags)))

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
             (org-roam-file-p))
    (org-with-point-at 1
      (let* ((tags (roam-extras/get-filetags))
             (is-todo (roam-extras/todo-p)))
        (cond ((and is-todo (not (seq-contains-p tags roam-extras-todo-tag-name)))
               (roam-extras/add-filetag roam-extras-todo-tag-name))
              ((and (not is-todo) (seq-contains-p tags roam-extras-todo-tag-name))
               (roam-extras/del-filetag roam-extras-todo-tag-name)))))))

(defun roam-extras/todo-files ()
  "Return a list of roam files containing todo tag."
  (org-roam-db-sync)
  (let ((todo-nodes (seq-filter (lambda (n)
                                  (seq-contains-p (org-roam-node-tags n) roam-extras-todo-tag-name))
                                 (org-roam-node-list))))
    (seq-uniq (seq-map #'org-roam-node-file todo-nodes))))

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
