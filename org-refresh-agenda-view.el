;; ======== refresh org-mode agenda regularly
;; https://orgmode.org/worg/org-hacks.html#orgab827a7

(defun kiwon/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
