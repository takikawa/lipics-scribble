all:
	scribble --latex example.scrbl
	sed -i'' "s/usenames,dvipsnames//" example.tex
	pdflatex example.tex
