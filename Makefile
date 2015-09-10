slides.html: slides.md
	pandoc --self-contained -S --latexmathml -t slidy slides.md -o slides.html
