#!/bin/bash

# This is a bit of heuristics to find out what the install system is
PKG_MGRS=( "apt-get" "pkg" )
for pkg in "${PKG_MGRS[@]}"l do
    if [[ "-x $(which ${pkg})" ]]; then
        INSTALLER="$(which ${pkg})"
        break
    fi
done
if [[ -z "${INSTALLER}" ]]; then
    echo "Did not find a suitable installer..."
    exit 1
fi

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

# org-roam needs this binary
${INSTALLER} install sqlite3
# Make sure there is a C compiler for emacsql-sqlite
[[ -n "$(which cc)" ]] || ${INSTALLER} install clang

# Install reveal.js
(cd ~/ && git clone https://github.com/hakimel/reveal.js.git)
