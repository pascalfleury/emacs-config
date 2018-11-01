# emacs-config
My personal Emacs configs, consisting mostly of configuration of free
tools.

## INSTALL

Usually, I sync this in /home/$USER/Emacs and then have ~/.emacs
consisting of these two lines:

  (load-file "~/Emacs/melpa.el")
  (load-file "~/Emacs/personal.el")

### EXPECTED PACKAGES

Absolutely needed is the 'use-package' package.

I usually install these through ELPA

 - ledger-mode
 - org
 - org-gcal
 - org-plus-contrib [deprecated?]
 - org-ehtml
 - multiple-cursors
 - wgrep
 - column-marker
 - unicode-escape
 - writeroom-mode
 - web-mode

### UNDER REVIEW
 - ox-reveal  (and installation, see https://github.com/yjwen/org-reveal#set-the-location-of-revealjs)
  - yankpad + yasnippet

### USEFUL PACKAGES

  - icicles
  - tj3-mode
  - ace-jump-mode
  - ledger-mode
  - hc-zenburn-theme
  - plantuml-mode
