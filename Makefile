FIG_FILES := $(patsubst %.R,%.pdf,$(wildcard ./fig/*.R))
TAB_FILES := $(patsubst %.R,%.tex,$(wildcard ./tab/*.R))

lasso-boot.pdf: lasso-boot.tex main.tex $(FIG_FILES) $(TAB_FILES)
	cleantex -beq lasso-boot.tex

## Run R files for figures
%.pdf: %.R
	@Rscript $< > /dev/null 2>&1

## Run R files for tables
%.tex: %.R
	@Rscript $< > /dev/null 2>&1
