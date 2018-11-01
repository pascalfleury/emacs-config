;; Dynamically adjust tag position
;; https://orgmode.org/worg/org-hacks.html#org0560357

(defun ba/org-adjust-tags-column-reset-tags ()
  "In org-mode buffers it will reset tag position according to
`org-tags-column'."
  (when (and
         (not (string= (buffer-name) "*Remember*"))
         (eql major-mode 'org-mode))
    (let ((b-m-p (buffer-modified-p)))
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (command-execute 'outline-next-visible-heading)
            ;; disable (message) that org-set-tags generates
            (flet ((message (&rest ignored) nil))
              (org-set-tags 1 t))
            (set-buffer-modified-p b-m-p))
        (error nil)))))

(defun ba/org-adjust-tags-column-now ()
  "Right-adjust `org-tags-column' value, then reset tag position."
  (set (make-local-variable 'org-tags-column)
       (- (- (window-width) (length org-ellipsis))))
  (ba/org-adjust-tags-column-reset-tags))

(defun ba/org-adjust-tags-column-maybe ()
  "If `ba/org-adjust-tags-column' is set to non-nil, adjust tags."
  (when ba/org-adjust-tags-column
    (ba/org-adjust-tags-column-now)))

(defun ba/org-adjust-tags-column-before-save ()
  "Tags need to be left-adjusted when saving."
  (when ba/org-adjust-tags-column
     (setq org-tags-column 1)
     (ba/org-adjust-tags-column-reset-tags)))

(defun ba/org-adjust-tags-column-after-save ()
  "Revert left-adjusted tag position done by before-save hook."
  (ba/org-adjust-tags-column-maybe)
  (set-buffer-modified-p nil))

;; between invoking org-refile and displaying the prompt (which
;; triggers window-configuration-change-hook) tags might adjust,
;; which invalidates the org-refile cache
(defadvice org-refile (around org-refile-disable-adjust-tags)
  "Disable dynamically adjusting tags"
  (let ((ba/org-adjust-tags-column nil))
    ad-do-it))
(ad-activate 'org-refile)

;; Now set it up
(setq ba/org-adjust-tags-column t)
;; automatically align tags on right-hand side
;; TODO(fleury): Does not seem to work as of 2017/12/18
;; Seems to work again 2018/11/01
(add-hook 'window-configuration-change-hook
          'ba/org-adjust-tags-column-maybe)
(add-hook 'before-save-hook 'ba/org-adjust-tags-column-before-save)
(add-hook 'after-save-hook 'ba/org-adjust-tags-column-after-save)
(add-hook 'org-agenda-mode-hook (lambda ()
                                  (setq org-agenda-tags-column (- (window-width)))))
