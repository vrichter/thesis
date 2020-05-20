SVGS=$(wildcard ../figures/*.svg)
FIGURES=$(patsubst ../figures/%,./%,$(patsubst %.svg,%.pdf_tex,$(SVGS)))

DOTS=$(wildcard ../figures/*.dot)
DOT_SVGS=$(patsubst ../figures/%,./%,$(patsubst %.dot,%.svg,$(DOTS)))
DOT_FIGURES=$(patsubst ../figures/%,./%,$(patsubst %.svg,%.pdf_tex,$(DOT_SVGS)))

%.pdf_tex %.pdf: %.svg
	inkscape --without-gui --export-dpi 300 --export-latex --export-pdf="$(patsubst %.pdf_tex,%.pdf,$@)" "$(patsubst %.pdf_tex,%.svg,$@)"


%.pdf_tex %.pdf: ../figures/%.svg
	inkscape --without-gui --export-dpi 300 --export-latex --export-pdf="$(patsubst %.pdf_tex,%.pdf,$@)" "$<"

%.svg: %.dot
	dot -Tsvg "-o$(patsubst %.pdf_tex,%.svg,$@)"  "$<"

%.svg: ../figures/%.dot
	dot -Tsvg "-o$(patsubst %.pdf_tex,%.svg,$@)"  "$<"

.PHONY: all
all: $(FIGURES) $(DOT_FIGURES)
	$(info -- figures generated)

.PHONY: clean
clean:
	rm -f *.pdf *.pdf_tex *.svg
