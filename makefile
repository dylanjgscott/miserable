LEXER=alex
PARSER=happy
HC=ghc

all: Lexer.hs Parser.hs Miserable

Lexer.hs: miserable.x
	$(LEXER) -o $@ $^

Parser.hs: miserable.y
	$(PARSER) -iparser.info $< -o $@

Miserable: Miserable.hs
	$(HC) --make $<

clean:
	rm Pexer.hs Parser.hs parser.info Miserable Miserable.hi


