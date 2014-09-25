LEXER=alex
PARSER=happy
HC=ghc

all: misery

Lexer.hs: Misery.x
	$(LEXER) -o $@ $<

Parser.hs: Misery.y
	$(PARSER) -o $@ $<

misery: Misery.hs Lexer.hs Parser.hs Semantic.hs Generator.hs Token.hs Program.hs
	$(HC) --make -o ../../$@ $<

clean:
	rm -f Token.hi Token.o
	rm -f Program.hi Program.o
	rm -f Semantic.hi Semantic.o
	rm -f Generator.hi Generator.o
	rm -f Lexer.hs Lexer.hi Lexer.o
	rm -f Parser.hs Parser.hi Parser.o
	rm -f Misery.hi Misery.o ../../misery
