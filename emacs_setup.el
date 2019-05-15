;; This file replaces itself with the actual configuration at first run.

(require 'org) ; We can't tangle without org!

(find-file "~/Emacs/emacs_setup.org") ; Open the configuration
(org-babel-tangle)                    ; tangle it
(load-file "~/Emacs/emacs_setup.el")  ; load it
(byte-compile-file "emacs_setup.el")  ; finally byte-compile it
