# [[file:emacs_setup.org::*Script to bootstrap the whole thing on a new system][Script to bootstrap the whole thing on a new system:1]]
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

# Maybe this is a new install, .emacs does not exist
test -e ~/.emacs || touch ~/.emacs

# Initial tangle of files, saying no to vterm compilation
emacs --batch --load "${GIT_ROOT}/lisp/first_time_tangle.el"

# Add the load-file as the first thing in the user's ~/.emacs
# If not yet added.
declare lines=$(grep ';; dot_emacs.el' ~/.emacs | wc -l)
if (( lines < 1 )); then
  echo ";; dot_emacs.el" > ~/.emacs.new
  cat "${GIT_ROOT}/dot_emacs.el" >> ~/.emacs.new
  cat ~/.emacs >> ~/.emacs.new
  mv ~/.emacs.new ~/.emacs
  echo "Added loading the config in your ~/.emacs"
else
  echo "Config in your ~/.emacs already set up!"
fi

# Install system dependencies from the tangled script
echo "Installing dependencies"
bash ${GIT_ROOT}/install_deps.sh

echo "Cleanup"
rm ${GIT_ROOT}/dot_emacs.el
rm ${GIT_ROOT}/install_deps.sh
# Script to bootstrap the whole thing on a new system:1 ends here
