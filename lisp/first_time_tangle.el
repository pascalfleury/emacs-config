;; This is the initial state of the file to be loaded.
;; It will replace itself with the actual configuration at first run.

(require 'org) ; We can't tangle without org!

(defvar config-base)
(setq config-base (expand-file-name "../emacs_setup" ;; because we're in the 'lisp' dir.
                                    (file-name-directory
				      (or load-file-name buffer-file-name))))
(message "Tangling '%s' ..." (concat config-base ".org"))
(find-file (concat config-base ".org"))  ; Open the configuration
(org-babel-tangle)                       ; tangle it
