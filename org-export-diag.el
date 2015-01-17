;;; org-export-diag -- blockdiag, seqdiag, actdiag, nwdiag export for Org-mode

;;; Code:
(require 'org)

(defgroup org-export-diag nil
  "Options for org-export-diag."
  :tag "Org Export diag"
  :group 'org-export)

(defcustom org-export-diag-blockdiag-command "blockdiag"
  "/usr/local/bin/seqdiag")
(defcustom org-export-diag-seqdiag-command "seqdiag"
  "Path to seqdiag command.")
(defcustom org-export-diag-actdiag-command "actdiag"
  "Path to actdig command.")
(defcustom org-export-diag-nwdiag-command "nwdiag"
  "Path to nwdiag command.")

(org-export-blocks-add-block '(blockdiag org-export-blocks-format-blockdiag nil))
(org-export-blocks-add-block '(seqdiag org-export-blocks-format-seqdiag nil))
(org-export-blocks-add-block '(actdiag org-export-blocks-format-actdiag nil))
(org-export-blocks-add-block '(nwdiag org-export-blocks-format-nwdiag nil))

(defun org-export-blocks-format-blockdiag (body &rest headers)
  (apply 'org-export-blocks-format-diag org-export-diag-blockdiag-command body headers))
(defun org-export-blocks-format-seqdiag (body &rest headers)
  (apply 'org-export-blocks-format-diag org-export-diag-seqdiag-command body headers))
(defun org-export-blocks-format-actdiag (body &rest headers)
  (apply 'org-export-blocks-format-diag org-export-diag-actdiag-command body headers))
(defun org-export-blocks-format-nwdiag (body &rest headers)
  (apply 'org-export-blocks-format-diag org-export-diag-nwdiag-command body headers))

(defun org-export-blocks-format-diag (diag-command body &rest headers)
  "Pass block BODY to the blockdiag utility creating an image.
Specify the path at wich the image should be saved as the first
element of headers, any additional elements of headers will be
passed to blockdiag utility as command line arguments."
  (let* ((args (if (cdr headers) (mapconcat 'identity (cdr headers) " ")))
         (data-file (make-temp-file "org-diag"))
         (hash (progn
                 (set-text-properties 0 (length body) nil body)
                 (sha1 (prin1-to-string (list body args)))))
         (raw-out-file (if headers (car headers)))
         (out-file-parts (if (string-match "\\(.+\\)\\.\\([^\\.]+\\)$" raw-out-file)
                             (cons (match-string 1 raw-out-file)
                                   (match-string 2 raw-out-file))
                           (cons raw-out-file "png")))
         (out-file (concat (car out-file-parts) "_" hash "." (cdr out-file-parts))))
    (unless (executable-find diag-command)
      (error (format "Could not find %s" diag-command)))
    (setq body (if (string-match "^\\([^:\\|:[^ ]\\)" body)
                   body
                 (mapconcat (lambda (x) (substring x (if (> (length x) 1) 2 1)))
                            (org-split-string body "\n")
                            "\n")))
    (prog1
        (cond
         ((member org-export-current-backend '(html latex docbook))
          (unless (file-exists-p out-file)
            (mapc
             (lambda (file)
               (when (and (string-match (concat (regexp-quote (car out-file-parts))
                                                "_\\([[:alnum:]]+\\)\\."
                                                (regexp-quote (cdr out-file-parts)))
                                        file)
                          (= (length (match-string 1 out-file)) 40))
                 (delete-file (expand-file-name file
                                                (file-name-directory out-file)))))
             (directory-files (or (file-name-directory out-file)
                                  default-directory)))
            (with-temp-file data-file (insert body))
            (message (concat diag-command " " args " " data-file " -o " out-file))
            (shell-command (concat diag-command " " args " " data-file " -o " out-file)))
          (format "\n[[file:%s]]\n" out-file))
         (t (concat
             "\n#+BEGIN_EXAMPLE\n"
             body (if (string-match "\n$" body) "" "\n")
             "#+END_EXAMPLE\n"))))))

(provide 'org-export-diag)
