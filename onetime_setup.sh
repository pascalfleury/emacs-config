#!/bin/bash

# Make git ignore the tangled & updated emacs_setup.el
GIT_ROOT=$(dirname $0)
(cd ${GIT_ROOT} && git update-index --skip-worktree emacs_setup.el)

# Maybe this is a new install, .emacs does not exist
test -e ~/.emacs || touch ~/.emacs

# Add the load-file as the first thing in the user's ~/.emacs
declare lines=$(grep emacs_setup ~/.emacs | wc -l)
if (( lines < 1 )); then
  echo "Added loading the config in your ~/.emacs"
  cat > ~/.emacs.new <<EOF
;; Loads PAF's emacs setup with bootstrap
(load-file "~/Emacs/emacs_setup.el")

EOF
  cat ~/.emacs >> ~/.emacs.new
  mv ~/.emacs.new ~/.emacs
else
  echo "Config in your ~/.emacs already set up!"
fi
