FIGURES=$(wildcard data/*.tex)

.DEFAULT_GOAL := all

export max_print_line=1048576
export PDFLATEX_EXTRA_ARGS='-interaction=nonstopmode'

.PHONY: generated
generated:
	@$(MAKE) -C generated
	@$(MAKE) -C data
	@$(MAKE) -C languagetool

.PHONY: prepare
prepare: generated
	$(info input prepared)



diss.pdf: *.tex */*.tex generated $(FIGURES)
	latexmk -pdflua

.PHONY: all
all: thesis.pdf
	$(info build finished)

.PHONY: clean
clean:
	latexmk -C
	@$(MAKE) -C generated clean
