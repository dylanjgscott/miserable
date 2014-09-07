LEXER=alex
PARSER=happy
HC=ghc

all: lexer.hs parser.hs

lexer.hs: miserable.x
	$(LEXER) -o $@ $<

parser.hs: miserable.y
	$(PARSER) -iparser.info $< -o $@

clean:
	rm lexer.hs parser.hs parser.info
