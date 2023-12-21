PDF_FILES := $(patsubst %.R,%.pdf,$(wildcard ./fig/*.R))
TEX_FILES := $(patsubst %.R,%.tex,$(wildcard ./latex/*.R))

lasso-boot.pdf: lasso-boot.tex main.tex $(PDF_FILES) $(TEX_FILES)
	cleantex -beq lasso-boot.tex

## Run R files for pdfs
%.pdf: %.R
	@Rscript $< > /dev/null 2>&1

## Run R files for latex
%.tex: %.R
	@Rscript $< > /dev/null 2>&1
