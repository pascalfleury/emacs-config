(require 'package)
(package-initialize)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                        ))

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(defmacro add-hook-run-once (hook function &optional append local)
  "Like add-hook, but remove the hook after it is called"
  (let ((sym (make-symbol "#once")))
    `(progn
       (defun ,sym ()
         (remove-hook ,hook ',sym ,local)
         (funcall ,function))
       (add-hook ,hook ',sym ,append ,local))))

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name "~/Emacs/emacs_setup.org"))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file "~/Emacs/emacs_setup.el"))))

(add-hook 'after-save-hook 'tangle-init)

(global-set-key (kbd "C-p") nil) ;; was 'previous-line'

(defvar locate-dominating-stop-dir-regexp
  "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

(setq browse-url-generic-program (executable-find "google-chrome")
  browse-url-browser-function 'browse-url-generic)

(use-package unicode-escape
  :ensure t
  :init
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(set-language-environment "UTF-8")

(global-auto-revert-mode 1)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(winner-mode 1)

(use-package eyebrowse
  :ensure t)

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key [M-f8] 'toggle-maximize-buffer)

(global-font-lock-mode t)

(when (display-graphic-p)
  (set-background-color "#ffffff")
  (set-foreground-color "#141312"))

(setq frame-title-format "emacs @ %b - %f")
(when window-system
  (mwheel-install)  ;; enable wheelmouse support by default
  (set-selection-coding-system 'compound-text-with-extensions))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(column-number-mode 1)
(setq visible-bell t)
(setq scroll-step 1)
(setq-default transient-mark-mode t)  ;; highlight selection

(use-package nyan-mode
  :ensure t
  :bind ("C-p n" . 'nyan-mode))

(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")

(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "orange"
           (if overwrite-mode "red"
             "green"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

(global-set-key [f3] 'start-kbd-macro)
(global-set-key [f4] 'end-kbd-macro)
(global-set-key [f5] 'call-last-kbd-macro)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-M-i") 'iedit-mode)

(global-set-key (kbd "C-c <C-left>")  'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>")    'windmove-up)
(global-set-key (kbd "C-c <C-down>")  'windmove-down)
(global-set-key (kbd "C-c C-g") 'goto-line)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C->" . 'mc/mark-all-like-this)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . 'ace-jump-mode)
         ("C-c DEL" . 'ace-jump-mode-pop-mark)))

(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package column-marker
  :config
  (add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 80)))
  :bind ("C-c m" . 'column-marker-1))

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(autoload 'comment-out-region "comment" nil t)
(global-set-key (kbd "C-c q") 'comment-out-region)

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun fleury/sort-and-uniquify-region ()
  "Remove duplicates and sort lines in region."
  (interactive)
  (sort-lines nil (region-beginning) (region-end))
  (uniquify-region-lines (region-beginning) (region-end)))

(global-set-key (kbd "C-p s") 'fleury/sort-and-uniquify-region)

(use-package yasnippet
  :ensure t)

(use-package yankpad
  :ensure t
  :init
  (setq yankpad-file "~/OrgFiles/yankpad.org")
  :config
  (bind-key "<f7>" 'yankpad-map))

(defun set-selective-display-dlw (&optional level)
  "Fold text indented more than the cursor.
   If level is set, set the indent level to level.
   0 displays the entire buffer."
  (interactive "P")
  (set-selective-display (or level (current-column))))

(global-set-key "\C-x$" 'set-selective-display-dlw)

(global-set-key (kbd "C-c C-n") 'linum-mode)

(use-package git-gutter-fringe+
:bind ("C-c g" . 'git-gutter+-mode))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package annotate
  :ensure t
  :bind ("C-c C-A" . 'annotate-annotate)  ;; for ledger-mode
  :config
  (add-hook 'org-mode 'annotate-mode)
  (add-hook 'csv-mode 'annotate-mode)
  (add-hook 'c-mode 'annotate-mode)
  (add-hook 'c++-mode 'annotate-mode)
  (add-hook 'sh-mode 'annotate-mode)
  (add-hook 'ledger-mode 'annotate-mode)
;;;  (define-globalized-minor-mode global-annotate-mode annotate-mode
;;;    (lambda () (annotate-mode 1)))
;;;  (global-annotate-mode 1)
  )

(use-package icicles
  :ensure t
  :config
  (icy-mode 1))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package emmet-mode
:ensure t)

(use-package rainbow-mode
:ensure t)

(use-package tj3-mode)

(use-package writeroom-mode
  :init
  (global-set-key (kbd "C-p w") 'writeroom-mode))

(use-package wgrep
  :ensure t)

(defun single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

(defun paf/cleanup-ledger-buffer ()
  "Cleanup the ledger file"
  (interactive)
  (delete-trailing-whitespace)
  (single-lines-only)
  (ledger-mode-clean-buffer)
  (ledger-sort-buffer))

(use-package ledger-mode
  :ensure t
  :mode "\\.ledger\\'"
  :bind ("<f6>" . 'paf/cleanup-ledger-buffer)
  :config
  (setq ledger-reconcile-default-commodity "CHF"))

(require 'org)
(if (not (boundp 'org-directory))
    (setq org-directory "~/OrgFiles"))

(defun org-relative-file (filename)
  "Compute an expanded absolute file path for org files"
  (expand-file-name filename org-directory))

(defun my-org-inherited-no-file-tags ()
  (let ((tags (org-entry-get nil "ALLTAGS" 'selective))
        (ltags (org-entry-get nil "TAGS")))
    (mapc (lambda (tag)
            (setq tags
                  (replace-regexp-in-string (concat tag ":") "" tags)))
          (append org-file-tags (when ltags (split-string ltags ":" t))))
    (if (string= ":" tags) nil tags)))

(defadvice org-archive-subtree
    (around my-org-archive-subtree-low-level activate)
  (let ((tags (my-org-inherited-no-file-tags))
        (org-archive-location
         (if (save-excursion (org-back-to-heading)
                             (> (org-outline-level) 1))
             (concat (car (split-string org-archive-location "::"))
                     "::* "
                     (car (org-get-outline-path)))
           org-archive-location)))
    ad-do-it
    (with-current-buffer (find-file-noselect (org-extract-archive-file))
      (save-excursion
        (while (org-up-heading-safe))
        (org-set-tags-to tags)))))

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

(defun kiwon/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))

(defun update-agenda-if-visible ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (org-agenda-redo))))

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;;(org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )

(load-file "~/Emacs/org-collector.el")

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(add-hook 'org-mode-hook
    (lambda ()
            (local-set-key (kbd "C-<up>") 'org-move-subtree-up)
            (local-set-key (kbd "C-<down>") 'org-move-subtree-down)))

(setq org-hide-leading-stars 't)
(setq org-log-done 't)
(setq org-startup-folded 't)
(setq org-startup-indented 't)
(setq org-startup-folded 't)
(setq org-ellipsis "...")
; Don't really like the new bullets though.
;;(use-package 'org-bullets
;;  :config
;;  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-habit
  :config
  (setq org-habit-graph-column 45)
  (setq org-habit-preceding-days 90)
  (setq org-habit-following-days 30)
  (setq org-habit-show-habits-only-for-today nil))

(setq org-babel-sh-command "bash")

(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)
(setq org-clock-int-drawer "CLOCK")

(setq org-mobile-directory (org-relative-file "Mobile"))
(setq org-mobile-inbox-for-pull (org-relative-file "mobileorg.org"))

(defun org-get-first-agenda-file ()
  (interactive)
  (find-file (elt org-agenda-files 0)))
(global-set-key [f12] 'org-get-first-agenda-file)
; F12 on Mac OSX displays the dashboard....
(global-set-key [C-f12] 'org-get-first-agenda-file)

(use-package org-ehtml
  :config
  (setq org-ehtml-docroot (expand-file-name org-directory))
  (setq org-ehtml-everything-editable t)
  (setq org-ehtml-allow-agenda t))

(defun fleury/start-web-server ()
  (interactive)
  (ws-start org-ehtml-handler 55555))
(global-set-key (kbd "<M-f12>") 'fleury/start-web-server)

(setq org-link-abbrev-alist
      '(("b" . "http://b/")
        ("go" . "http://go/")
        ("cl" . "http://cr/")))

(use-package org-secretary
:config
(setq org-sec-me "paf")
(setq org-tag-alist '(("PRJ" . ?p)
                      ("DESIGNDOC" . ?D)
                      ("Milestone" . ?m)
                      ("DESK" . ?d)
                      ("HOME" . ?h)
                      ("VC" . ?v))))

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)

(setq org-global-properties
      '(("Effort_ALL". "0 0:10 0:30 1:00 2:00 4:00 8:00 16:00")))
(setq org-columns-default-format
      "%TODO %30ITEM %3PRIORITY %6Effort{:} %10DEADLINE")

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

;; '(org-agenda-date ((t (:inherit org-agenda-structure :background "pale green" :foreground "black" :weight bold))) t)
;; '(org-agenda-date-weekend ((t (:inherit org-agenda-date :background "light blue" :weight bold))) t)
;; '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "yellow" :weight bold))))

(setq org-agenda-current-time-string ">>>>>>>>>> NOW <<<<<<<<<<")

;; will refresh it only if already visible
;;(run-at-time nil 180 'update-agenda-if-visible)
(add-hook 'org-mode-hook
          (lambda () (run-at-time nil 180 'kiwon/org-agenda-redo-in-other-window)))

;; Make this happen only if we open an org file.
(add-hook 'org-mode-hook
          (lambda () (run-with-idle-timer 600 t 'jump-to-org-agenda)))

(add-hook 'org-mode-hook
    (lambda () (run-with-idle-timer 60 t 'org-save-all-org-buffers)))

(use-package org-duration
  :config
  (setq org-duration-units
    `(("min" . 1)
      ("h" . 60)
      ("d" . ,(* 60 8))
      ("w" . ,(* 60 8 5))
      ("m" . ,(* 60 8 5 4))
      ("y" . ,(* 60 8 5 4 10)))
     )
  (org-duration-set-regexps))

(setq org-default-notes-file (org-relative-file "refile.org"))

(setq org-capture-templates
      `(("a" "Action Item" entry (file+headline ,(org-relative-file "refile.org") "Tasks")
             "* AI %?\n  %U")
        ("m" "Meeting" entry (file+headline ,(org-relative-file "refile.org") "Meetings")
             "* %U  :MTG:\n %^{with}p\n%?")
        ("n" "Note" entry (file+headline ,(org-relative-file "refile.org") "Notes")
             "* %?\n%U")
        ("j" "Journal" entry (file+datetree ,(org-relative-file "journal.org"))
             "* %?\n  %U")))

; show up to 2 levels for refile targets, in all agenda files
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
(setq org-log-refile t)  ;; will add timestamp when refiled.
; from: http://doc.norang.ca/org-mode.html
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

(setq org-confirm-babel-evaluate 'nil) ; Don't ask before executing

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (python . t)
   (ledger . t)
   ;;(sh . t)
   (latex . t)
   (plantuml . t)
   (shell . t)
  ))

(use-package ox-odt)
(use-package ox-reveal) ; under review
(use-package ox-taskjuggler)

(setq org-plantuml-jar-path (expand-file-name "~/Apps/plantuml.jar"))

(use-package iimage
  :config
  (add-to-list 'iimage-mode-image-regex-alist
               (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                             "\\)\\]")  1))

  (defun org-toggle-iimage-in-org ()
    "display images in your org file"
    (interactive)
    (if (face-underline-p 'org-link)
        (set-face-underline 'org-link nil)
      (set-face-underline 'org-link t))
    (iimage-mode 'toggle))

  (add-hook 'org-mode-hook (lambda ()
                             ;; display images
                             (local-set-key "\M-I" 'org-toggle-iimage-in-org)
                            )))
