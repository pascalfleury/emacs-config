#!/bin/bash
set -e

# Trick to make it work on Termux
which "ls" || pkg install debianutils

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
