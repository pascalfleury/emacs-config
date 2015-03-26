;; This file contains mostly org-config specific to me, myself and I.
;; ==================================================================

;; Make the calendar day info a bit more visible.
;; Move this into the custom-set-faces in ~/.emacs
;; '(org-agenda-date ((t (:inherit org-agenda-structure :background "pale green" :foreground "black" :weight bold))) t)
;; '(org-agenda-date-weekend ((t (:inherit org-agenda-date :background "light blue" :weight bold))) t)


(require 'org)
(require 'org-secretary)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; some config for display
(setq org-hide-leading-stars 't)
(setq org-log-done 't)
(setq org-startup-folded 't)
(setq org-startup-indented 't)
(setq org-startup-folded 't)

(setq org-ellipsis "...")
; Don't rally like the new bullets...cc
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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
        ("cl" . "http://cr/")))

(setq org-sec-me "paf")
(setq org-tag-alist '(("PRJ" . ?p) ("Milestone" . ?m) ("DESK" . ?d) ("HOME" . ?h) ("VC" . ?v)))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(C@)" "DEFERRED(D@)" "SOMEDAY(S!)" "FAILED(F!)" "REFILED(R!)")
        (sequence "TASK(m!)" "|" "DONE(d!)" "CANCELLED(C@)" )))

(setq org-tags-exclude-from-inheritance '("PRJ")
      org-use-property-inheritance '("PRIORITY")
      org-stuck-projects '("+PRJ/-DONE-CANCELLED"
                           ("TODO" "NEXT" "STARTED" "TASK") ()))

 (setq org-agenda-custom-commands
       '(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
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
        ("STARTED" . (:foreground "orange" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("FLAG_GATED" . (:foreground "orange" :weight bold))
        ("SOMEDAY" . (:foreground "steelblue" :weight bold))
        ("MAYBE" . (:foreground "steelblue" :weight bold))
        ("AI" . (:foreground "black" :weight bold))
        ("NEW" . (:foreground "orange" :weight bold))
        ("RUNNING" . (:foreground "orange" :weight bold))
        ("WORKED" . (:foreground "green" :weight bold))
        ("FAILED" . (:foreground "red" :weight bold))
        ("REFILED" . (:foreground "gray"))
       ))

;; capture and refile stuff
; show up to 2 levels for refile targets, in all agenda files
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
(setq org-default-notes-file "~/OrgFiles/refile.org")

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
   (sh . t)
   (latex . t)
   ;;(shell . t)
  ))

;; Don't ask before executing
(setq org-confirm-babel-evaluate 'nil)

; Add the ODT as an export format
(eval-after-load "org"
  '(require 'ox-odt nil t))
