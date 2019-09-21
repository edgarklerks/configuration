#!/bin/zsh
cd /tmp
NAME="$1"
EXPR="$2"
doc=$(cat <<EOF
\\documentclass[preview]{standalone}
\\\usepackage{mathtools}
\\\begin{document}
$ ${EXPR} $
\\\end{document}
EOF
)

echo $doc | pdflatex 1> /dev/null

convert -density 800 texput.pdf -quality 100 $HOME/equations/$NAME.png

print "Outputted to: $HOME/equations/$NAME.png"
rm texput.{pdf,log,aux}
