CHAPTERS=$(wildcard ../chapters/*.tex)
FRONTBACK=$(wildcard ../frontback/*tex)
PLAIN=$(patsubst ../chapters/%,./%,$(patsubst %.tex,%.txt,$(CHAPTERS))) $(patsubst ../frontback/%,./%,$(patsubst %.tex,%.txt,$(FRONTBACK)))

texidote.jar:
	wget https://github.com/sylvainhalle/textidote/releases/download/v0.8.1/textidote.jar

%.txt: ../chapters/%.tex texidote.jar
	java -jar textidote.jar --clean --read-all "$<" > "$@"

%.txt: ../frontback/%.tex texidote.jar
	java -jar textidote.jar --clean --read-all "$<" > "$@"

.PHONY: all
all: $(PLAIN)
	$(info -- text extracted)

.PHONY: clean
clean:
	rm -f *.txt
