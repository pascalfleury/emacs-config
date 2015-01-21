(defvar locate-dominating-stop-dir-regexp
        "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

;; Setup MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
)

(defvar locate-dominating-stop-dir-regexp
        "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

;; ================ My own stuff
(require 'uniquify)   ;; make buffer names more unique
;;(require 'icicles)    ;; enhanced minibuffer completion

;; Load my org stuff
(load-file "~/Emacs/org-mode-hacks.el")
(load-file "~/Emacs/my-org-mode-config.el")

(load-file "~/Emacs/color_cursors.el")
(load-file "~/Emacs/selective_display.el")
(load-file "~/Emacs/code-hacks.el")

;; ===== Use auto-revert, which reloads a file if it's updated on disk
;;       and not modified in the buffer.
(global-auto-revert-mode 1)

;; Simple cleanup of #include/typedef/using blocks.
(global-set-key [M-f5] 'fleury/sort-and-uniquify-region)

;; Toggle temporary buffer maximization
(global-set-key [M-f8] 'toggle-maximize-buffer)

; ===== Configure the shortcuts for multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

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
(global-set-key [M-down]    'next-error)
(global-set-key [M-up]      '(lambda () (interactive) (next-error -1)))

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

;; turn on font-lock mode
(global-font-lock-mode t)

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; goto line function C-c C-g
(global-set-key [ (control c) (control g) ] 'goto-line)

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
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key (quote [f9]) (quote compile))
(display-time)

(put 'narrow-to-region 'disabled nil)

;; easy commenting out of lines
(autoload 'comment-out-region "comment" nil t)
(global-set-key "\C-cq" 'comment-out-region)

;; Show column number at bottom of screen
(column-number-mode 1)
