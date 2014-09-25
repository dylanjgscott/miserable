LEXER=alex
PARSER=happy
HC=ghc

all: company

AsmLexer.hs: Company.x
	$(LEXER) -o $@ $<

AsmParser.hs: Company.y
	$(PARSER) -o $@ $<

company: Company.hs AsmLexer.hs AsmParser.hs Interpreter.hs AsmToken.hs
	$(HC) --make -o ../../$@ $<

clean:
	rm -f Interpreter.hi Interpreter.o
	rm -f AsmToken.hi AsmToken.o
	rm -f Assembly.hi Assembly.o
	rm -f AsmLexer.hs AsmLexer.hi AsmLexer.o
	rm -f AsmParser.hs AsmParser.hi AsmParser.o
	rm -f Company.hi Company.o ../../company
