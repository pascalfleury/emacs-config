#!/bin/bash
source "$(dirname $0)"/install.sh

APPDIR=${HOME}/Apps
[[ -d "${APPDIR}" ]] || mkdir -p "${APPDIR}"

# Install TaskJuggler
if [[ "$(uname -m)" == "x86_64" ]]; then
  if [[ "$(which tj3)" == "" ]]; then
    case "$(uname)" in
      Darwin)  brew install ruby ; sudo gem install taskjuggler ;;
      *)       install_pkg tj3 ;;
    esac
  fi
fi

install_pkg gnuplot

install_pkg cargo
install_pkg gcc
install_pkg g++

install_pkg -x rg ripgrep

# Install the vterm native library
install_pkg libvterm-dev

# Install PanDoc
if [[ "$(uname -m)" == "x86_64" ]]; then
  install_pkg pandoc
fi

# org-roam needs this binary
if [[ "$(uname -o)" == "Android" ]]; then
    install_pkg -x sqlite3 sqlite
else
    install_pkg -x sqlite3 sqlite3
fi
# Make sure there is a C compiler for emacsql-sqlite
[[ -n "$(which cc)" ]] || install_pkg -x cc clang

# Install reveal.js
if [[ -d "${APPDIR}/reveal.js" ]]; then
  echo "Reveal already installed"
else
  (cd ${APPDIR} && git clone https://github.com/hakimel/reveal.js.git)
fi

# Get a version of the PlantUML jar file.
install_pkg -x dot graphviz  # for some diagrams
install_pkg -x wget wget

JARFILE="${APPDIR}/plantuml.jar"
INSTALLED_JAR=$(readlink -f "${JARFILE}")
declare -i last_modified=$(stat -c %Y "${INSTALLED_JAR}" 2>/dev/null || echo "0")
age_in_days=$(( ( $(date +%s) - last_modified ) / 86400 ))

if [[ ! -e "${JARFILE}" ]] || (( age_in_days > 90 )); then
  LATEST_JAR="$(wget -O - 'https://plantuml.com/en/download' | grep GPL | grep jar | sed -e 's,.* href=",,' -e 's,".*,,' | head -n 1)"
  (cd "$(dirname "${JARFILE}")" && wget -O $(basename "${LATEST_JAR}") "${LATEST_JAR}")
  echo "New installed version is $(basename "${LATEST_JAR}") [age was ${age_in_days} days]"
  ( cd "$(dirname "${JARFILE}")" && ln -s -f $(basename "${LATEST_JAR}") $(basename "${JARFILE}"))
  ls -l "${JARFILE}"
else
  echo "Latest version is ${INSTALLED_JAR} [age is ${age_in_days} days]"
fi

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
