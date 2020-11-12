#!/bin/bash
# Make git ignore the tangled & updated emacs_setup.el
if [[ -z "$(which git)" ]]; then
  echo "You will need 'git' to be installed !"
  exit 1
fi
if  [[ -z "$(which emacs)" ]]; then
  echo "You might need 'emacs' for this to be useful !"
  exit 1
fi

GIT_ROOT=$(dirname $0)
(cd ${GIT_ROOT} && git update-index --skip-worktree emacs_setup.el)

# Maybe this is a new install, .emacs does not exist
test -e ~/.emacs || touch ~/.emacs

# Initial tangle of files.
emacs --batch --load ${GIT_ROOT}/emacs_setup.el
GENERATED=/tmp  # where it creates files

# Add the load-file as the first thing in the user's ~/.emacs
# If not yet added.
declare lines=$(grep ';; dot_emacs.el' ~/.emacs | wc -l)
if (( lines < 1 )); then
  echo "Added loading the config in your ~/.emacs"
  echo ";; dot_emacs.el" > ~/.emacs.new
  cat ${GENERATED}/dot_emacs.el >> ~/.emacs.new
  cat ~/.emacs >> ~/.emacs.new
  mv ~/.emacs.new ~/.emacs
else
  echo "Config in your ~/.emacs already set up!"
fi

# Install system dependencies
bash ${GENERATED}/install_deps.sh
