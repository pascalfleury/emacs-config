(defun set-selective-display-dlw (&optional level)
  "Fold text indented more than the cursor.
   If level is set, set the indent level to level.
   0 displays the entire buffer."
  (interactive "P")
  (set-selective-display (or level (current-column))))

(global-set-key "\C-x$" 'set-selective-display-dlw)
