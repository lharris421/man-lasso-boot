PDF_FILES := $(patsubst %.R,%.pdf,$(wildcard ./fig/*.R))

lasso-boot.pdf: lasso-boot.tex main.tex $(PDF_FILES)
	cleantex -beq lasso-boot.tex

## Run R files
%.pdf: %.R
	Rscript $<

