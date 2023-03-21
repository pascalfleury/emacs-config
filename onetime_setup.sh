#!/bin/bash
# Make git ignore the tangled & updated emacs_setup.el
GIT_ROOT=$(dirname $0)

# Ensure modules are loaded.
(cd ${GIT_ROOT} && git submodule init && git submodule update)

source ${GIT_ROOT}/bash/install.sh

if [[ -z "$(which git)" ]]; then
    echo "You might need 'git' for this to work ! (how did you get here?)"
    exit 1
fi
if  [[ -z "$(which emacs)" ]]; then
    echo "You might need 'emacs' for this to be useful ! Installing..."
    install_pkg -x emacs emacs
fi

# Maybe this is a new install, .emacs does not exist
for file in ~/.emacs ~/.emacs.d/custom.el; do
    test -e ${file} || mkdir -p $(dirname ${file}) && touch ${file}
done

# Initial tangle of files, saying no to vterm compilation
echo "Initial tangling..."
(cd ${GIT_ROOT} && emacs --batch --load "lisp/first_time_tangle.el")

# Add the load-file as the first thing in the user's ~/.emacs
# If not yet added.
declare lines=$(grep ';; lisp/dot_emacs.el' ~/.emacs | wc -l)
if (( lines < 1 )); then
    echo "Setup .emacs ..."
    echo ";; lisp/dot_emacs.el" > ~/.emacs.new
    cat "${GIT_ROOT}/lisp/dot_emacs.el" >> ~/.emacs.new
    cat ~/.emacs >> ~/.emacs.new
    mv ~/.emacs.new ~/.emacs
    echo "Added loading the config in your ~/.emacs"
else
    echo "Config in your ~/.emacs already set up!"
fi

# Install system dependencies from the tangled script
echo "Checking dependencies"
bash ${GIT_ROOT}/bash/install_deps.sh

# Load the init, let it install whatever is missing.
echo "Get Emacs to load fist time..."
emacs --batch --load "~/.emacs"
