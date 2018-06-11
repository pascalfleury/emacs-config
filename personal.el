(defvar locate-dominating-stop-dir-regexp
        "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

;; ================ Testing ground
(add-hook 'org-mode-hook 
          (lambda ()
            (local-set-key (kbd "C-<up>") 'org-move-subtree-up)
            (local-set-key (kbd "C-<down>") 'org-move-subtree-down)))

;; ================ My own stuff
(require 'uniquify)   ;; make buffer names more unique
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Make Emacs request UTF-8 first when pasting stuff.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(require 'unicode-escape) ;; does this interfere with pasting ?

;; enhanced minibuffer completion
(require 'icicles)    
(icy-mode 1)

;; web-mode for Polymer editing
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(when (require 'tj3-mode nil 'noerror))

;; Enable winner-mode
;; Navigate buffer-window configs with C-c left and C-c right.
(when (fboundp 'winner-mode) (winner-mode 1))

(global-set-key (kbd "C-c <C-left>")  'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>")    'windmove-up)
(global-set-key (kbd "C-c <C-down>")  'windmove-down)

;; Load my org stuff
(load-file "~/Emacs/my-org-mode-config.el")
(load-file "~/Emacs/org-mode-hacks.el")
(load-file "~/Emacs/org-collector.el")

(load-file "~/Emacs/color_cursors.el")
(load-file "~/Emacs/selective_display.el")
(load-file "~/Emacs/code-hacks.el")

;; ===== Use auto-revert, which reloads a file if it's updated on disk
;;       and not modified in the buffer.
(global-auto-revert-mode 1)
(put 'upcase-region 'disabled nil)

;; Simple cleanup of #include/typedef/using blocks.
(global-set-key (kbd "C-p") nil) ;; was 'previous-line'
(global-set-key (kbd "C-p s") 'fleury/sort-and-uniquify-region)

(require 'writeroom-mode)
(global-set-key (kbd "C-p w") 'writeroom-mode)

;; Toggle temporary buffer maximization
(global-set-key [M-f8] 'toggle-maximize-buffer)

;; Mark the 80 col boundary
(require 'column-marker)
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 80)))
;; Use `C-c m' interactively to highlight with face `column-marker-1'.
(global-set-key [?\C-c ?m] 'column-marker-1)

; ===== Configure the shortcuts for multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

(require 'wgrep)
(global-set-key (kbd "C-M-i") 'iedit-mode)

;; ==== Configure my ledger mode
(require 'ledger-mode)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

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

(define-key ledger-mode-map (kbd "<f6>") 'paf/cleanup-ledger-buffer)

(setq ledger-reconcile-default-commodity "CHF")

;; ==== Let's one jump around text
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c DEL") 'ace-jump-mode-pop-mark)

;; ==== switch from header to implementation file quickly
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

; By the way, the following bindings are often very useful in
; compilation/grep/lint mode:
;(global-set-key [M-down]    'next-error)
;(global-set-key [M-up]      '(lambda () (interactive) (next-error -1)))

; play with macros
(global-set-key [f3] 'start-kbd-macro)
(global-set-key [f4] 'end-kbd-macro)
(global-set-key [f5] 'call-last-kbd-macro)

;; Increase/decrease text size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; modify some of the looks
(global-font-lock-mode t)
(setq frame-title-format "emacs - %b")
(when (display-graphic-p) ;; used when in konsole mode
  (set-background-color "#ffffff")
  (set-foreground-color "#141312"))

;; key stroke to change color theme
;(require 'color-theme)
;(color-theme-initialize)
;(setq color-theme-is-global t)
;
;(defun fleury/set-my-color-theme ()
;  (interactive)
;  '(load-theme 'hc-zenburn))
;(global-set-key (kbd "S-<f11>") 'fleury/set-my-color-theme)

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; goto line function C-c C-g
(global-set-key (kbd "C-c C-g") 'goto-line)

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions))

(setq browse-url-generic-program (executable-find "google-chrome")
      browse-url-browser-function 'browse-url-generic)

;; suppress sound bell
(setq visible-bell t)

;; Clean scrolling (aaaaaah)
(setq scroll-step 1)
;; (keyboard-translate ?\C-m ?\C-l)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (quote [f9]) (quote compile))
(display-time)

(put 'narrow-to-region 'disabled nil)

;; easy commenting out of lines
(autoload 'comment-out-region "comment" nil t)
(global-set-key (kbd "C-c q") 'comment-out-region)

;; Show column number at bottom of screen
(column-number-mode 1)
