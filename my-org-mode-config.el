;; This file contains mostly org-config specific to me, myself and I.
;; ==================================================================

;; Make the calendar day info a bit more visible.
;; Move this into the custom-set-faces in ~/.emacs
;; '(org-agenda-date ((t (:inherit org-agenda-structure :background "pale green" :foreground "black" :weight bold))) t)
;; '(org-agenda-date-weekend ((t (:inherit org-agenda-date :background "light blue" :weight bold))) t)
;; '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "yellow" :weight bold))))


(require 'org-secretary)
(require 'org)
(require 'org-habit)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; some config for display
(setq org-hide-leading-stars 't)
(setq org-log-done 't)
(setq org-startup-folded 't)
(setq org-startup-indented 't)
(setq org-startup-folded 't)

(setq org-habit-graph-column 45)
(setq org-habit-preceding-days 90)
(setq org-habit-following-days 30)
(setq org-habit-show-habits-only-for-today nil)

(setq org-ellipsis "...")
; Don't really like the new bullets...cc
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; A more visible current-time marker in the agenda
(setq org-agenda-current-time-string ">>>>>>>>>> NOW <<<<<<<<<<")

(setq org-babel-sh-command "bash")

;; clock stuff into a drawer
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)
(setq org-clock-int-drawer "CLOCK")

;; config for org-mobile-*
(setq org-directory "~/OrgFiles")
(setq org-mobile-directory "~/OrgFiles/Mobile")
(setq org-mobile-inbox-for-pull (concat org-directory "/mobileorg.org"))

;; F12 open the first agenda file
(defun org-get-first-agenda-file ()
  (interactive)
  (find-file (elt org-agenda-files 0)))
(global-set-key [f12] 'org-get-first-agenda-file)
; F12 on Mac OSX displays the dashboard....
(global-set-key [C-f12] 'org-get-first-agenda-file)

;; This will start serving the org files through the emacs-based webbrowser
;; when pressing M-f12 (on localhost:55555)
(setq org-ehtml-docroot (expand-file-name "~/OrgFiles"))
(setq org-ehtml-everything-editable t)
(setq org-ehtml-allow-agenda t)
(require 'org-ehtml)

(defun fleury/start-web-server ()
  (interactive)
  (ws-start org-ehtml-handler 55555))
(global-set-key (kbd "<M-f12>") 'fleury/start-web-server)

(setq org-link-abbrev-alist
      '(("b" . "http://b/")
        ("go" . "http://go/")
        ("cl" . "http://cr/")))

(setq org-sec-me "paf")
(setq org-tag-alist '(("PRJ" . ?p) ("DESIGNDOC" . ?D) ("Milestone" . ?m) ("DESK" . ?d) ("HOME" . ?h) ("VC" . ?v)))

;; track task dependencies, and dim them in in the agenda.
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)

(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "WAITING(w!)" "AI(a!)" "|" "DONE(d!)" "CANCELLED(C@)" "DEFERRED(D@)" "SOMEDAY(S!)" "FAILED(F!)" "REFILED(R!)")
        (sequence "APPLIED(A!)" "WAITING(w!)" "ACCEPTED" "|" "REJECTED" "PUBLISHED")
        (sequence "TASK(m!)" "|" "DONE(d!)" "CANCELLED(C@)" )))

(setq org-tags-exclude-from-inheritance '("PRJ")
      org-use-property-inheritance '("PRIORITY")
      org-stuck-projects '("+PRJ/-DONE-CANCELLED"
			   ; it is considered stuck if there is no next action
                           (;"TODO"
			    "NEXT" "STARTED" "TASK") ()))

(setq org-agenda-custom-commands
       '(("t" "Hot Today" ((agenda "" ((org-agenda-span 'day)))
                           (tags-todo "/NEXT")
                           (tags-todo "-dowith={.+}/STARTED")
                           (tags-todo "-dowith={.+}/WAITING")))
         ("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
         ("N" "Next actions" tags-todo
          "-personal-doat={.+}-dowith={.+}/!-TASK-TODO"
          ((org-agenda-todo-ignore-scheduled t)))
         ("h" "Work todos" tags-todo
          "-personal-doat={.+}-dowith={.+}/!-TASK"
          ((org-agenda-todo-ignore-scheduled t)))
         ("H" "All work todos" tags-todo "-personal/!-TASK-CANCELLED"
          ((org-agenda-todo-ignore-scheduled nil)))
         ("A" "Work todos with doat or dowith" tags-todo
          "-personal+doat={.+}|dowith={.+}/!-TASK"
          ((org-agenda-todo-ignore-scheduled nil)))
         ("j" "TODO dowith and TASK with"
         ((org-sec-with-view "TODO dowith")
          (org-sec-where-view "TODO doat")
          (org-sec-assigned-with-view "TASK with")
          (org-sec-stuck-with-view "STUCK with")
          (todo "STARTED")))
         ("J" "Interactive TODO dowith and TASK with"
          ((org-sec-who-view "TODO dowith")))))

(setq org-todo-keyword-faces '(
        ("TODO" . (:foreground "purple" :weight bold))
        ("TASK" . (:foreground "steelblue" :weight bold))
        ("NEXT" . (:foreground "red" :weight bold))
        ("STARTED" . (:foreground "darkgreen" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("FLAG_GATED" . (:foreground "orange" :weight bold))
        ("SOMEDAY" . (:foreground "steelblue" :weight bold))
        ("MAYBE" . (:foreground "steelblue" :weight bold))
        ("AI" . (:foreground "red" :weight bold))
        ("NEW" . (:foreground "orange" :weight bold))
        ("RUNNING" . (:foreground "orange" :weight bold))
        ("WORKED" . (:foreground "green" :weight bold))
        ("FAILED" . (:foreground "red" :weight bold))
        ("REFILED" . (:foreground "gray"))
        ; For publications
        ("APPLIED" . (:foreground "orange" :weight bold))
        ("ACCEPTED" . (:foreground "orange" :weight bold))
        ("REJECTED" . (:foreground "red" :weight bold))
        ("PUBLISHED" . (:foreground "green" :weight bold))
        ))

;; Capture and refile stuff
; show up to 2 levels for refile targets, in all agenda files
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
(setq org-default-notes-file "~/OrgFiles/refile.org")
(setq org-log-refile t)  ;; will add timestamp when refiled.

;; some templates that I think are useful
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/OrgFiles/refile.org" "Tasks")
             "* TODO %?\n  %U")
        ("m" "Meeting" entry (file+headline "~/OrgFiles/refile.org" "Meetings")
             "* %U  :MTG:\n %^{with}p\n%?")
        ("n" "Note" entry (file+headline "~/OrgFiles/refile.org" "Notes")
             "* %?\n%U")
        ("j" "Journal" entry (file+datetree "~/OrgFiles/journal.org")
             "* %?\n  %U")))

; from: http://doc.norang.ca/org-mode.html
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; What kind of code block languages do I need
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (dot . t)
   (gnuplot . t)
   (python . t)
   (ledger . t)
   ;;(sh . t)
   (latex . t)
   (plantuml . t)
   (shell . t)
  ))

;; Tell where PlantUML is to be found
(setq org-plantuml-jar-path (expand-file-name "~/Apps/plantuml.jar"))

;; Don't ask before executing
(setq org-confirm-babel-evaluate 'nil)

(setq org-global-properties
      '(("Effort_ALL". "0 0:10 0:30 1:00 2:00 4:00 8:00 16:00")))
(setq org-columns-default-format
      "%TODO %30ITEM %3PRIORITY %6Effort{:} %10DEADLINE")

; Add the ODT as an export format
(eval-after-load "org"
  '(require 'ox-odt nil t))
(require 'ox-reveal) ; under review
;(require 'ox-taskjuggler)

;; Make the display of images a simple key-stroke away.
(require 'iimage)
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                           "\\)\\]")  1))

(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (iimage-mode 'toggle))

(add-hook 'org-mode-hook (lambda ()
                           ;; display images
                           (local-set-key "\M-I" 'org-toggle-iimage-in-org)
                          ))

;; Make this happen only if we open an org file.
;;(add-hook 'org-mode-hook (lambda () (run-with-idle-timer 600 t 'jump-to-org-agenda)))
;;(add-hook 'org-mode-hook (lambda () (run-at-time "10 min" 300 'update-agenda-if-visible)))
