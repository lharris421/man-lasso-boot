TAB_FILES := $(patsubst %.R,%.tex,$(wildcard ./tab/*.R))
R_SCRIPTS := $(wildcard ./fig/*.R)
FIG_FILES := $(shell for script in $(R_SCRIPTS); do fig/target $$script || exit 1; done)

# Make tables
%.tex: %.R
	@Rscript $< > /dev/null 2>&1

# Make figures
%.pdf: %.R
	@Rscript $< > /dev/null 2>&1
%.png: %.R
	@Rscript $< > /dev/null 2>&1

# Make document
lasso-boot.pdf: lasso-boot.tex main.tex $(FIG_FILES) $(TAB_FILES)
	cleantex -btq lasso-boot.tex
