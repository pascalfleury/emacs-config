#!/bin/bash
source $(dirname $0)/bash/install.sh

# Install TaskJuggler
  if [[ "$(uname -m)" == "x86_64" ]]; then
    install_pkg tj3
  fi

if [[ "$(uname)" == "Darwin" ]]; then
  install_pkg -x ag the_silver_searcher
else
  install_pkg -x ag silversearcher-ag
fi

# Needed to compile vterm first time
if [[ "$(uname -o)" == "Android" ]]; then
  install_pkg -x libtool libtool
else
  install_pkg -x libtool libtool-bin
fi
install_pkg -x cmake cmake
install_pkg -x perl perl

# Also amend the bash config
cat >> ${HOME}/.bashrc <<EOF
# Setup Emacs's VTerm communication
if [[ "\${INSIDE_EMACS}" = 'vterm' ]] \\
    && [[ -n "\${EMACS_VTERM_PATH}" ]] \\
    && [[ -f "\${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh" ]]; then
        source "\${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh"
fi
EOF

# org-roam needs this binary
if [[ "$(uname -o)" == "Android" ]]; then
    install_pkg -x sqlite3 sqlite
else
    install_pkg -x sqlite3 sqlite3
fi
# Make sure there is a C compiler for emacsql-sqlite
[[ -n "$(which cc)" ]] || install_pkg -x cc clang

# wget used for org-board archiving.
install_pkg -x wget wget

# Install reveal.js
if [[ -d "${HOME}/reveal.js" ]]; then
  echo "Reveal already installed"
else
  (cd ~/ && git clone https://github.com/hakimel/reveal.js.git)
fi

# Get a version of the PlantUML jar file.
install_pkg -x dot graphviz  # for some diagrams
install_pkg -x wget wget
URL='http://sourceforge.net/projects/plantuml/files/plantuml.jar/download'
DIR="${HOME}/Apps"
if [[ ! -e "${DIR}/plantuml.jar" ]]; then
    [[ -d "${DIR}" ]] || mkdir -p "${DIR}"
    (cd "${DIR}" && wget -O plantuml.jar "${URL}")
    ls -l "${DIR}/plantuml.jar"
fi

#!/bin/bash
set -e

# Trick to make it work on Termux
#which "ls" || pkg install debianutils

# This is a bit of heuristics to find out what the install system is
# They are attempted in this order, put the least likely first.
declare -a PKG_MGRS=("pkg" "brew" "apt-get")

PKG_PREFIX_apt_get="sudo"
PKG_POSTFIX_apt_get="-y"

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

# This is the function to call to install anything. It can optionally
# check for a binary and avoid installing if it's found.  install_pkg
# [-x <binary>] <package>
function install_pkg() {
    if [[ "$1" == "-x" ]]; then
        local binary="$(which $2)"
        if [[ -n "${binary}" && -x "${binary}" ]]; then
            echo "Found $2 (${binary}), nothing to install for $3."
            return
        fi
        shift 2
    fi

    local token=$(echo -n ${INSTALLER} | tr -c '0-9a-zA-Z_' '_')
    local prefix_var="PKG_PREFIX_${token}"
    local postfix_var="PKG_POSTFIX_${token}"

    echo "Trying: ${INSTALLER} install $*"
    ${!prefix_var} $(which ${INSTALLER}) ${!postfix_var} install "$@"
}
