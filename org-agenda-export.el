;; that's the export function to update the agenda view
(defun dmg-org-update-agenda-file (&optional force)
  (interactive)
  (save-excursion
    (save-window-excursion
      (let ((file "~/www/agenda/agenda.html"))
        (org-agenda-list)
        (org-write-agenda file)))))


;; format to output the agenda
(setq org-agenda-exporter-settings
      '((ps-number-of-columns 2)
        (ps-portrait-mode t)
        (org-agenda-add-entry-text-maxlines 5)
        (htmlize-output-type 'font)))
