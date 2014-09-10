LEXER=alex
PARSER=happy
HC=ghc

all: Miserable

Lexer.hs: Miserable.x
	$(LEXER) -o $@ $<

Parser.hs: Miserable.y
	$(PARSER) -iParser.info -o $@ $<

Miserable: Miserable.hs Lexer.hs Parser.hs
	$(HC) --make -o $@ $<

clean:
	rm -f Token.hi Token.o
	rm -f Program.hi Program.o
	rm -f Lexer.hs Lexer.hi Lexer.o
	rm -f Parser.info Parser.hs Parser.hi Parser.o
	rm -f Miserable.hi Miserable.o Miserable
