SVGS=$(wildcard ../figures/*.svg)
FIGURES=$(patsubst ../figures/%,./%,$(patsubst %.svg,%.pdf_tex,$(SVGS)))

DOTS=$(wildcard ../figures/*.dot)
DOT_SVGS=$(patsubst ../figures/%,./%,$(patsubst %.dot,%.svg,$(DOTS)))
DOT_FIGURES=$(patsubst ../figures/%,./%,$(patsubst %.svg,%.pdf_tex,$(DOT_SVGS)))

DEFENCE_CSRA_LAYERS=g10 g11837 layer1 layer34 layer40  layer2 layer10 layer24  g12835 g12901  layer6  layer3 layer25  g5699 layer30 layer23
DEFENCE_FFORMATION_COSTS_LAYERS=layer1 layer3 layer4 layer6
DEFENCE_MEKA_LAYERS=layer_base layer_agents layer1 layer2 layer3 layer4 layer5 layer6
DEFENCE_ROLE_LAYERS=image groups speaker addressee sideparticipant
DEFENCE_NNL_LAYERS=lstm dense t1 t2

.DEFAULT_GOAL := all

%.pdf_tex %.pdf: %.svg
	inkscape --without-gui --export-dpi 300 --export-latex --export-pdf="$(patsubst %.pdf_tex,%.pdf,$@)" "$(patsubst %.pdf_tex,%.svg,$@)"


%.pdf_tex %.pdf: ../figures/%.svg
	inkscape --without-gui --export-dpi 300 --export-latex --export-pdf="$(patsubst %.pdf_tex,%.pdf,$@)" "$<"

%.png: ../figures/%.svg
	inkscape --without-gui --export-dpi 300 --export-png="$@" "$<"

%.svg: %.dot
	dot -Tsvg "-o$(patsubst %.pdf_tex,%.svg,$@)"  "$<"

%.svg: ../figures/%.dot
	dot -Tsvg "-o$(patsubst %.pdf_tex,%.svg,$@)"  "$<"

csra-map-defence-%.png:  ../figures/csra-map-defence.svg
	inkscape --without-gui --export-area-page --export-id-only --export-id="$(patsubst csra-map-defence-%.png,%,$@)" --export-dpi 48 --export-png="$@" "../figures/csra-map-defence.svg"

fformation-costs-%.pdf:  ../figures/fformation-costs.svg
	inkscape --without-gui --export-area-page --export-id-only --export-id="$(patsubst fformation-costs-%.pdf,%,$@)" --export-dpi 300 --export-latex --export-pdf="$@" "../figures/fformation-costs.svg"

addressee-meka-process-%.pdf:  ../figures/addressee-meka-process.svg
	inkscape --without-gui --export-area-page --export-id-only --export-id="$(patsubst addressee-meka-process-%.pdf,%,$@)" --export-dpi 300 --export-latex --export-pdf="$@" "../figures/addressee-meka-process.svg"

defence_role_annotations_real-%.pdf:  ../figures/defence_role_annotations_real.svg
	inkscape --without-gui --export-area-page --export-id-only --export-id="$(patsubst defence_role_annotations_real-%.pdf,%,$@)" --export-dpi 300 --export-latex --export-pdf="$@" "../figures/defence_role_annotations_real.svg"

nnl-defence-%.pdf:  ../figures/nnl-defence.svg
	inkscape --without-gui --export-area-page --export-id-only --export-id="$(patsubst nnl-defence-%.pdf,%,$@)" --export-dpi 300 --export-latex --export-pdf="$@" "../figures/nnl-defence.svg"

%.mkv:
	youtube-dl "https://www.youtube.com/watch?v=$(subst .mkv,,$@)" -o "%(id)s"
	test -f $(subst mkv,mp4,$@) &&  mv -f $(subst mkv,mp4,$@) $@; true

aido-short.mp4: W58U6mmPrmk.mkv
	ffmpeg -y -i $< -ss 18 -t 19 -an $@

aido-short.jpg: aido-short.mp4
	ffmpeg -y -sseof -3 -i $<  -update 1 -q:v 1 $@

elevator.mp4: BgRoiTWkBHU.mkv
	ffmpeg -y -i $<  -ss 18 -t 18 -an $@

elevator.jpg: elevator.mp4
	#ffmpeg -y -ss 5 -i $<  -update 1 -q:v 1 $@
	ffmpeg -i $< -vf "select=eq(n\,20)" -vframes 1 elevator.jpg


.PHONY: all
all: $(FIGURES) $(DOT_FIGURES) rw-wordcloud.png $(patsubst %,csra-map-defence-%.png,$(DEFENCE_CSRA_LAYERS)) $(patsubst %,fformation-costs-%.pdf,$(DEFENCE_FFORMATION_COSTS_LAYERS)) $(patsubst %,addressee-meka-process-%.pdf,$(DEFENCE_MEKA_LAYERS)) $(patsubst %,defence_role_annotations_real-%.pdf,$(DEFENCE_ROLE_LAYERS)) $(patsubst %,nnl-defence-%.pdf,$(DEFENCE_NNL_LAYERS)) aido-short.jpg elevator.jpg
	$(info -- figures generated)

.PHONY: clean
clean:
	rm -f *.pdf *.pdf_tex *.svg *.jpg *.mkv *.mp4 *.png
