#!/bin/bash
git update-index --skip-worktree emacs_setup.el

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

sudo apt-get install silversearcher-ag
