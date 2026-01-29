cargo run itself manual -o ./manual/functions.tex
cargo run itself graph -o ./manual/graph.pdf
cd ./manual
pdflatex manual.tex
bibtex manual.aux
pdflatex manual.tex
pdflatex manual.tex
