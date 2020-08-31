;;; org-secretary.el --- Team management with org-mode
;; Copyright (C) 2010-2014 Juan Reyero
;;
;; Author: Juan Reyero <juan _at_ juanreyero _dot_ com>
;; Keywords: outlines, tasks, team, management
;; Homepage: http://juanreyero.com/article/emacs/org-teams.html
;; Version: 0.02
;; Heavily adapted by fleury@users.sourceforge.net
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; THis file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This module implements helper functions for team management.  It
;; makes it easy to keep track of the work of several people.  It
;; keeps context (with whom and where you are) and allows you to use
;; it to metadata to your notes, and to query the tasks associated
;; with the people you are with and the place.
;;
;; See http://juanreyero.com/article/emacs/org-teams.html for a full
;; explanation and configuration instructions.
;;
;;; Configuration
;;;;;;;;;;;;;;;;;
;;
;; In short; your todos use the TODO keyword, your team's use TASK.
;; Your org-todo-keywords should look something like this:
;;
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
;;         (sequence "TASK(f)" "|" "DONE(d)")
;;         (sequence "MAYBE(m)" "|" "CANCELLED(c)")))
;;
;; It helps to distinguish them by color, like this:
;;
;; (setq org-todo-keyword-faces
;;       '(("TODO" . (:foreground "DarkOrange1" :weight bold))
;;         ("MAYBE" . (:foreground "sea green"))
;;         ("DONE" . (:foreground "light sea green"))
;;         ("CANCELLED" . (:foreground "forest green"))
;;         ("TASK" . (:foreground "blue"))))
;;
;; If you want to keep track of stuck projects you should tag your
;; projects with :prj:, and define:
;;
;; (setq org-tags-exclude-from-inheritance '("prj")
;;       org-stuck-projects '("+prj/-MAYBE-DONE"
;;                            ("TODO" "TASK") ()))
;;
;; Define a tag that marks TASK entries as yours:
;;
;; (setq paf-sec-me "paf")
;;
;; Finally, you add the special views to your org-agenda-custom-commands:
;;
;; (setq org-agenda-custom-commands
;;       '(("h" "Work todos" tags-todo
;;          "-personal-doat={.+}-dowith={.+}/!-TASK"
;;          ((org-agenda-todo-ignore-scheduled t)))
;;         ("H" "All work todos" tags-todo "-personal/!-TASK-MAYBE"
;;          ((org-agenda-todo-ignore-scheduled nil)))
;;         ("A" "Work todos with doat or dowith" tags-todo
;;          "-personal+doat={.+}|dowith={.+}/!-TASK"
;;          ((org-agenda-todo-ignore-scheduled nil)))
;;         ("j" "TODO dowith and TASK with"
;;          ((paf-sec-with-view "TODO dowith")
;;           (paf-sec-where-view "TODO doat")
;;           (paf-sec-assigned-with-view "TASK with")
;;           (paf-sec-stuck-with-view "STUCK with")))
;;         ("J" "Interactive TODO dowith and TASK with"
;;          ((paf-sec-who-view "TODO dowith")))))
;;
;;; Usage
;;;;;;;;;
;;
;;  Do C-c w to say with whom you are meeting (a space-separated list
;;  of names). Maybe do also C-c W to say where you are.  Then do C-c a
;;  j to see:
;;     - Todo items defined with TODO (ie, mine) in which the
;;       =with= property matches any of the people with me.
;;     - Todo items defined with TODO in which the =doat= property
;;       matches my current location.
;;     - Todo items defined with TASK that are tagged with the name
;;       of any of the people with me (this is, assigned to them).
;;     - Stuck projects tagged with the name of the people with me.
;;
;; Use C-c j to add meta-data with the people with me, the
;; location and the time to entries.

(require 'org)

(defcustom paf-sec-me nil
  "Tag that defines TASK todo entries associated to me")

(defcustom paf-sec-with-property "with"
  "Name of the property used to store who you are with.")

(defcustom paf-sec-where-property "where"
  "Name of the property used to store where a task should be done.")

;; ===== state variables
(defvar paf-sec-with nil
  "Value of the :with: property when doing an
   paf-sec-tag-entry. Change it with paf-sec-set-with,
   set to C-c w.  Defaults to paf-sec-me")

(defvar paf-sec-where ""
  "Value of the :where: property when doing an
   paf-sec-tag-entry. Change it with paf-sec-set-with,
   set to C-c W")

(defvar paf-sec-with-history '()
  "History list of :with: properties")

(defvar paf-sec-where-history '()
  "History list of :where: properties")

(defun paf-sec-set-with ()
  "Changes the value of the paf-sec-with variable for use in the
   next call of paf-sec-tag-entry.  Leave it empty to default to
   paf-sec-me (you)."
  (interactive)
  (setq paf-sec-with (let ((w (read-string "With: " nil
                                           'paf-sec-with-history "")))
                       (if (string= w "")
                           nil
                         w))))

(defun paf-sec-set-where ()
  "Changes the value of the paf-sec-where variable for use
   in the next call of paf-sec-tag-entry."
  (interactive)
  (setq paf-sec-where
        (read-string "Where: " nil
                     'paf-sec-where-history "")))

(defun paf-sec-set-with-metadata ()
  "Sets the value of the with property."
  (interactive)
  (let ((do-with
         (read-string "Do with: "
                      nil 'paf-sec-with-history "")))
    (unless (string= do-with "")
      (org-entry-put nil paf-sec-with-property do-with))))

(defun paf-sec-set-where-metadata ()
  "Sets the value of the where property."
  (interactive)
  (let ((do-at (read-string "Do at: "
                            nil 'paf-sec-where-history "")))
    (unless (string= do-at "")
      (org-entry-put nil paf-sec-where-property do-at))))

(defun paf-sec-tag-entry ()
  "Adds a :with: property with the value of paf-sec-with if
   defined, a :where: property with the value of paf-sec-where
   if defined, and an :on: property with the current time."
  (interactive)
  (save-excursion
    (org-entry-put nil "on" (format-time-string
                             (org-time-stamp-format 'long)
                             (current-time)))
    (unless (string= paf-sec-where "")
      (org-entry-put nil paf-sec-at-property paf-sec-where))
    (if paf-sec-with
        (org-entry-put nil paf-sec-with-property paf-sec-with))))

(defun paf-sec-replace-with-where (config)
  "Replaces $WITH and $WHERE with the respective values of
   :paf-sec-with: and :paf-sec-where:."
  (let ((with-replaced (replace-regexp-in-string "$WITH" paf-sec-with config))
        (all-replaced (replace-regexp-in-string "$WHERE" paf-sec-where with-replaced)))
    all-replaced))


;; ===== function to use in calendar
(defun paf-sec-use-with-where (fct config &optional options)
  "Replaces $WITH and $WHERE with the respective values of
   :paf-sec-with: and :paf-sec-where:, then calls the passed function"
  (let ((expanded-config (paf-sec-replace-with-where config)))
    (funcall fct expanded-config options)))

(provide 'paf-secretary)

;;; org-secretary.el ends here
