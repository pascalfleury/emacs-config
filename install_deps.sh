#!/bin/bash

# This is a bit of heuristics to find out what the install system is
PKG_MGRS=(   "apt-get" "pkg"     "apt"     )
PKG_PREFIX=( "sudo"    ""        "sudo"    )
PKG_CMD=(    "install" "install" "install" )
for pkg in "${PKG_MGRS[@]}"; do
    if [[ -x "$(which ${pkg})" ]]; then
        INSTALLER="${pkg}"
        break
    fi
done
if [[ -z "${INSTALLER}" ]]; then
    echo "Did not find a suitable installer (tried ${PKG_MGRS[@]})"
    exit 1
fi

function install_pkg() {
  echo "Trying to install package $*"
  ${PKG_PREFIX[$INSTALLER]} $(which ${INSTALLER}) ${PKG_CMD[$INSTALLER]} "$@"
}

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

# helm-ag uses this for faster grepping
install_pkg silversearcher-ag

# This can be used by helm-ag for faster grepping
install_pkg ripgrep

# org-roam needs this binary
install_pkg sqlite3
# Make sure there is a C compiler for emacsql-sqlite
[[ -n "$(which cc)" ]] || install_pkg clang

# wget used for org-board archiving.
install_pkg wget

# for all the native apps related to PDF tools
install_pkg pdf-tools
