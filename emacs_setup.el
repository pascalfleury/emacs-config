;; This file replaces itself with the actual configuration at first run.

(require 'org) ; We can't tangle without org!

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                        ))

(setq config_base (expand-file-name "emacs_setup"
				    (file-name-directory
				     (or load-file-name buffer-file-name))))
(find-file (concat config_base ".org"))        ; Open the configuration
(org-babel-tangle)                             ; tangle it
(load-file (concat config_base ".el"))         ; load it
(byte-compile-file (concat config_base ".el")) ; finally byte-compile it
