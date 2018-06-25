# emacs-config
My personal Emacs configs, consisting mostly of configuration of free
tools.

## INSTALL

Usually, I sync this in /home/$USER/Emacs and then have ~/.emacs
consisting of these two lines:

  (load-file "~/Emacs/melpa.el")
  (load-file "~/Emacs/personal.el")

### EXPECTED PACKAGES

I usually install these through ELPA

 - ledger-mode
 - org
 - org-gcal
 - org-plus-contrib [deprecated?]
 - org-ehtml
 - ace-jump-mode
 - multiple-cursors
 - icicles
 - wgrep
 - column-marker
 - unicode-escape
 - writeroom-mode
 - web-mode

### UNDER REVIEW
 - ox-reveal  (and installation, see https://github.com/yjwen/org-reveal#set-the-location-of-revealjs)

### USEFUL PACKAGES

  - hc-zenburn-theme
  - plantuml-mode
