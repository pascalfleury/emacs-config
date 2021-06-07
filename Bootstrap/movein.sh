#!/bin/bash

readonly HS_REPOS="${HOME}/.homesick/repos"

[ -d "${HS_REPOS}" ] || mkdir -p "${HS_REPOS}" \
&& cd "${HS_REPOS}" \
&& git clone https://github.com/Billiam/homeshick.git \
alias homeshick="${HS_REPOS}/homeshick/home/.homeshick"

homeshick clone https://github.com/pascalfleury/dotfiles.git
homeshick clone https://github.com/pascalfleury/shelltoys.git
