(require 'org)

;;(org-remember-insinuate)

;;(add-hook 'org-mode-hook
;;          (lambda ()
;;            (set-background-color "black")))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; some config for display
(setq org-hide-leading-stars 't)
(setq org-log-done 't)
(setq org-startup-folded 't)
(setq org-startup-indented 't)

(setq org-ellipsis "...")

(setq org-babel-sh-command "bash")

;; config for org-mobile-*
(setq org-directory "~/OrgFiles")
(setq org-mobile-directory "~/OrgFiles/Mobile")
(setq org-mobile-inbox-for-pull (concat org-directory "/mobileorg.org"))

;; Don't ask before executing
(setq org-confirm-babel-evaluate 'nil)

;; clock stuff into a drawer
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)
(setq org-clock-int-drawer "CLOCK")

(setq org-link-abbrev-alist
      '(("b" . "http://b/")
        ("cl" . "http://cr/")))

(setq org-sec-me "paf")

(setq org-tag-alist '(("PRJ" . ?p) ("DESK" . ?d) ("HOME" . ?h) ("VC" . ?v)))

;;(setq org-todo-keywords
;;      '((types "TODO(t!)" "TASK(m!)" "NEXT(n!)" "STARTED(s!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c@)" "DEFERRED(f@)" "SOMEDAY(S!)" "FAILED(F!)")))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c@)" "DEFERRED(f@)" "SOMEDAY(S!)" "FAILED(F!)")
        (sequence "TASK(m!)" "|" "DONE(d!)" "CANCELLED(c@)" )))

(setq org-tags-exclude-from-inheritance '("PRJ")
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
         ((org-sec-with-view "STARTED dowith")
           (org-sec-with-view "TODO dowith")
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
       ))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (dot . t)
   (gnuplot . t)
   (python . t)
   (sh . t)
  )) ; this line activates dot

; Add the ODT as an export format
(eval-after-load "org"
  '(require 'ox-odt nil t))
