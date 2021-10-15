;; This is the initial state of the file to be loaded.
;; It will replace itself with the actual configuration at first run.

(require 'org) ; We can't tangle without org!

(setq config_base (expand-file-name "emacs_setup"
				    (file-name-directory
				     (or load-file-name buffer-file-name))))
(find-file (concat config_base ".org"))  ; Open the configuration
(org-babel-tangle)                       ; tangle it
