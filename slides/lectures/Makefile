OUTDIR = $(shell Rscript -e "cat(fs::path(here::here('docs', fs::path_rel(getwd(), start=here::here()))))")
SLIDES := $(wildcard *.Rmd)
SLIDES_OUT = $(patsubst %.Rmd, $(OUTDIR)/%.html, $(SLIDES))
SLIDES_PDF = $(patsubst %.Rmd, $(OUTDIR)/%.pdf, $(SLIDES))

.PHONY: all
all: $(SLIDES_OUT)
.PHONY: pdf
pdf: $(SLIDES_PDF)

$(OUTDIR)/%.html: %.Rmd _output.yml ../templates/setup.Rmd
	Rscript -e "rmarkdown::render('$(*F).Rmd', output_yaml = '_output.yml')"
	rm -rf $(OUTDIR)/$(*F)_files
	mv $(*F).html $(OUTDIR)
	mv -f $(*F)_files $(OUTDIR)

$(OUTDIR)/%.pdf: $(OUTDIR)/%.html
	cp -r ../css $(OUTDIR)
	cp -r ../js $(OUTDIR)
	cp -r img $(OUTDIR)
	Rscript -e "pagedown::chrome_print('$(@D)/$(*F).html')"
	rm -rf css
	rm -rf js
	rm -rf img

.PHONY: files
files:
	@echo $(OUTDIR)
	@echo $(SLIDES_OUT)
	@echo $(SLIDES)

.PHONY: clean clean_cache
clean:
	@rm -rf $(SLIDES_OUT)
clean_cache:
	@rm -rf *cache
