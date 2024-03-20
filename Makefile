PDF_FILES := $(patsubst %.R,%.pdf,$(wildcard ./fig/*.R))
# TEX_FILES := $(patsubst %.R,%.tex,$(wildcard ./latex/*.R))
# $(TEX_FILES)
lasso-boot.pdf: lasso-boot.tex main.tex $(PDF_FILES)
	cleantex -beq lasso-boot.tex

## Run R files for pdfs
%.pdf: %.R
	@Rscript $< > /dev/null 2>&1
