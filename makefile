LEXER=alex
PARSER=happy
HC=ghc

all: Lexer.hs Parser.hs Miserable

Lexer.hs: Miserable.x
	$(LEXER) -o $@ $<

Parser.hs: Miserable.y
	$(PARSER) -iParser.info -o $@ $<

Miserable: Miserable.hs
	$(HC) --make -o $@ $<

clean:
	rm Lexer.hs Parser.hs Parser.info Miserable Miserable.hi
