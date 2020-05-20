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

%.pdf: %.tex */*.tex generated $(FIGURES)
	latexmk -pdflua "$<"

defence.pdfpc2: defence.pdfpc defence.pdf ./process-pdfpc.py
	cat defence.pdfpc | ./process-pdfpc.py > defence.pdfpc2

.PHONY: all
all: thesis.pdf defence.pdf defence.pdfpc2
	$(info build finished)

.PHONY: clean
clean:
	latexmk -C
	@$(MAKE) -C generated clean
