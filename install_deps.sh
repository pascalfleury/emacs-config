#!/bin/bash

# This is a bit of heuristics to find out what the install system is
declare -a PKG_MGRS=("apt-get" "pkg" "brew")

PKG_PREFIX_apt_get="sudo"

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
        shift 2
        if [[ -n "${binary}" && -x "${binary}" ]]; then
            echo "Found $2 (${binary}), nothing to install."
            return
        fi
    fi

    local token=$(echo -n ${INSTALLER} | tr -c '0-9a-zA-Z_' '_')
    local prefix_var="PKG_PREFIX_${token}"

    echo "Trying: ${INSTALLER} install $*"
    ${!prefix_var} $(which ${INSTALLER}) install "$@"
}

# helm-ag uses this for faster grepping
if [[ "$(uname)" == "Darwin" ]]; then
  install_pkg -x ag the_silver_searcher
else
  install_pkg -x ag silversearcher-ag
fi

# This can be used by helm-ag for faster grepping
install_pkg -x rg ripgrep

# org-roam needs this binary
install_pkg -x sqlite3 sqlite3
# Make sure there is a C compiler for emacsql-sqlite
[[ -n "$(which cc)" ]] || install_pkg -x cc clang

# wget used for org-board archiving.
install_pkg -x wget wget

# Get a version of the PlantUML jar file.
install_pkg -x wget wget

URL='http://sourceforge.net/projects/plantuml/files/plantuml.jar/download'
DIR="${HOME}/Apps"
if [[ ! -e "${DIR}/plantuml.jar" ]]; then
    [[ -d "${DIR}" ]] || mkdir -p "${DIR}"
    (cd "${DIR}" && wget -O plantuml.jar "${URL}")
    ls -l "${DIR}/plantuml.jar"
fi

# For all the native apps related to PDF tools
# I did not sintall it on Max OSX yet.
if [[ "$(uname)" != "Darwin" ]]; then
  install_pkg pdf-tools
fi
