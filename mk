#!/bin/sh

set -o errexit
set -o nounset
set -o noclobber

name=ta

# Re-run 3 times to get bibliographic changes through

[ -d gen ] || mkdir gen
dot2tex --cache --autosize --codeonly -t raw --usepdflatex flow.gv -o gen/flow.tex
R --no-save --restore <<EOF
Stangle("$name.Rnw")
Sweave("$name.Rnw")
EOF
pdflatex $name
bibtex $name
