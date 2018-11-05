;; This is only needed once, near the top of the file
(eval-when-compile (require 'use-package))

(defvar locate-dominating-stop-dir-regexp
  "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

;; ================ Testing ground
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-<up>") 'org-move-subtree-up)
            (local-set-key (kbd "C-<down>") 'org-move-subtree-down)))

;; ================ Stock Emacs config

;; Remove C-p that I want to use for *me* personally as a prefix.
(global-set-key (kbd "C-p") nil) ;; was 'previous-line'

;; no tabs, ever. No traling spaces either.
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(column-number-mode 1)
(setq visible-bell t)
(setq scroll-step 1)
(setq-default transient-mark-mode t)  ;; highlight selection

;; Enable winner-mode
;; Navigate buffer-window configs with C-c left and C-c right.
(winner-mode 1)
(global-font-lock-mode t)
(setq frame-title-format "emacs - %b")
(when (display-graphic-p) ;; used when in konsole mode
  (set-background-color "#ffffff")
  (set-foreground-color "#141312"))
(when window-system
  (mwheel-install)  ;; enable wheelmouse support by default
  (set-selection-coding-system 'compound-text-with-extensions))

(setq browse-url-generic-program (executable-find "google-chrome")
      browse-url-browser-function 'browse-url-generic)

;; ===== Use auto-revert, which reloads a file if it's updated on disk
;;       and not modified in the buffer.
(global-auto-revert-mode 1)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

; navigation
(global-set-key (kbd "C-c <C-left>")  'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>")    'windmove-up)
(global-set-key (kbd "C-c <C-down>")  'windmove-down)
(global-set-key (kbd "C-c C-g") 'goto-line)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

(global-set-key [M-f8] 'toggle-maximize-buffer)
(global-set-key (kbd "C-M-i") 'iedit-mode)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
; play with macros
(global-set-key [f3] 'start-kbd-macro)
(global-set-key [f4] 'end-kbd-macro)
(global-set-key [f5] 'call-last-kbd-macro)
;; Increase/decrease text size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; ==== switch from header to implementation file quickly
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; easy commenting out of lines
(autoload 'comment-out-region "comment" nil t)
(global-set-key (kbd "C-c q") 'comment-out-region)

;; ================ My own stuff
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Make Emacs request UTF-8 first when pasting stuff.
(use-package unicode-escape
  :ensure t
  :init
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; file-annotations store externally.
;; Seems to fail with args-out-of-range and then Emacs is confused.
(use-package annotate
  :ensure t
  :config
  (add-hook 'org-mode 'annotate-mode)
  (add-hook 'csv-mode 'annotate-mode)
  (add-hook 'c-mode 'annotate-mode)
  (add-hook 'c++-mode 'annotate-mode)
  (add-hook 'sh-mode 'annotate-mode)
;;;  (define-globalized-minor-mode global-annotate-mode annotate-mode
;;;    (lambda () (annotate-mode 1)))
;;;  (global-annotate-mode 1)
;;;  ; Remove it from some view, it interferes with the colors
;;;  (add-hook 'org-agenda-mode-hook (lambda () (annotate-mode 0)))
;;;  (add-hook 'magit-status-mode-hook (lambda () (annotate-mode 0)))
  )

;; Add the powerful Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; enhanced minibuffer completion if we have icicles
(use-package icicles
  :ensure t
  :config
  (icy-mode 1))

;; web-mode for Polymer editing
(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package tj3-mode) ;; TaskJuggler mode

(use-package writeroom-mode
  :init
  (global-set-key (kbd "C-p w") 'writeroom-mode))

;; Mark the 80 col boundary
(use-package column-marker
  :config
  (add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 80)))
  :bind ("C-c m" . 'column-marker-1))

; ===== Configure the shortcuts for multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C->" . 'mc/mark-all-like-this)))

(use-package wgrep
  :ensure t)

;; ==== Let's one jump around text
(use-package ace-jump-mode
  :bind (("C-c SPC" . 'ace-jump-mode)
         ("C-c DEL" . 'ace-jump-mode-pop-mark)))

;; Load my org stuff
(use-package org
  :config
  (require 'org-secretary)
  (require 'org-habit)

  (load-file "~/Emacs/my-org-mode-config.el")
  (load-file "~/Emacs/org-preserve-structure-in-archive.el")
  (load-file "~/Emacs/org-adjust-tags-on-right.el")

  (load-file "~/Emacs/org-refresh-agenda-view.el")
  ;; will refresh it only if already visible
  (run-at-time nil 180 'kiwon/org-agenda-redo-in-other-window)

  (load-file "~/Emacs/org-display-agenda-when-idle.el")
  ;; Make this happen only if we open an org file.
  (add-hook 'org-mode-hook
            (lambda () (run-with-idle-timer 600 t 'jump-to-org-agenda)))

  (load-file "~/Emacs/org-collector.el"))

(use-package yasnippet
  :ensure t)

(use-package yankpad
  :ensure t
  :init
  (setq yankpad-file "~/OrgFiles/yankpad.org")
  :config
  (bind-key "<f7>" 'yankpad-map))

;; some additional tooling
(load-file "~/Emacs/color_cursors.el")
(load-file "~/Emacs/selective_display.el")

;; Simple cleanup of #include/typedef/using blocks.
(load-file "~/Emacs/code-hacks.el")
(global-set-key (kbd "C-p s") 'fleury/sort-and-uniquify-region)

;; ==== Configure my ledger mode
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
