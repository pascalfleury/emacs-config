#!/bin/bash

# Make git ignore the tangled & updated emacs_setup.el
GIT_ROOT=$(dirname $0)
(cd ${GIT_ROOT} && git update-index --skip-worktree emacs_setup.el)

# Maybe this is a new install, .emacs does not exist
test -e ~/.emacs || touch ~/.emacs

# Add the load-file as the first thing in the user's ~/.emacs
# If not yet added.
declare lines=$(grep ';; dot_emacs.el' ~/.emacs | wc -l)
if (( lines < 1 )); then
  echo "Added loading the config in your ~/.emacs"
  echo ";; dot_emacs.el" > ~/.emacs.new
  cat ${GIT_ROOT}/dot_emacs.el >> ~/.emacs.new
  cat ~/.emacs >> ~/.emacs.new
  mv ~/.emacs.new ~/.emacs
else
  echo "Config in your ~/.emacs already set up!"
fi

# Make sure there is a C compiler for emacsql-sqlite
[[ -n "$(which cc)" ]] || pkg install clang

# Install reveal.js
(cd ~/ && git clone https://github.com/hakimel/reveal.js.git)
