;; Loads PAF's emacs setup with bootstrap
(load-file "~/Emacs/emacs_setup.el")

;; faces for general region highlighting zenburn is too low-key.
(custom-set-faces
 '(highlight ((t (:background "forest green"))))
 '(region ((t (:background "forest green")))))

;; Faces to make the calendar more colorful.
(custom-set-faces
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "yellow" :weight bold))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :background "pale green" :foreground "black" :weight bold))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :background "light blue" :weight bold)))))
