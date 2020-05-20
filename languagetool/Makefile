CHAPTERS=$(wildcard ../chapters/*.tex)
FRONTBACK=$(wildcard ../frontback/*tex)
PLAIN=$(patsubst ../chapters/%,./%,$(patsubst %.tex,%.txt,$(CHAPTERS))) $(patsubst ../frontback/%,./%,$(patsubst %.tex,%.txt,$(FRONTBACK)))

%.txt: ../chapters/%.tex
	java -jar textidote.jar --clean --read-all "$<" > "$@"

%.txt: ../frontback/%.tex
	java -jar textidote.jar --clean --read-all "$<" > "$@"

.PHONY: all
all: $(PLAIN)
	$(info -- text extracted)

.PHONY: clean
clean:
	rm -f *.txt
