all:
	scribble --latex example.scrbl
	sed -i'' "s/usenames,dvipsnames//" example.tex
	sed -i'' "s/.*ccaption.*//" example.tex
	pdflatex example.tex
