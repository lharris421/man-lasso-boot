# List all the .pdf files as dependencies
PDF_FILES := $(patsubst %.R,%.pdf,$(wildcard ./fig/*.R))

lasso-boot.pdf: lasso-boot.tex main.tex $(PDF_FILES)
#cleantex -beq lasso-boot
	pdflatex lasso-boot.tex

# Rule to run R scripts
%.pdf: %.R
	Rscript --vanilla $<

