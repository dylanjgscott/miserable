LEXER=alex
PARSER=happy
HC=ghc
TEST=tests
VPATH = tests

all: misery tester

Lexer.hs: Misery.x
	$(LEXER) -o $@ $<

Parser.hs: Misery.y
	$(PARSER) -iParser.info -o $@ $<

misery: Misery.hs Lexer.hs Parser.hs Semantic.hs
	$(HC) --make -o $@ $<

tester: Tester.hs ParserTester.hs LexerTester.hs
	$(HC) --make -o $(TEST)/$@ $< -itests/


clean:
	rm -f Token.hi Token.o
	rm -f Program.hi Program.o
	rm -f Semantic.hi Semantic.o
	rm -f Lexer.hs Lexer.hi Lexer.o
	rm -f Parser.info Parser.hs Parser.hi Parser.o
	rm -f Misery.hi Misery.o misery
	rm -f $(TEST)/Tester.hi $(TEST)/Tester.o $(TEST)/tester
	rm -f $(TEST)/LexerTester.hi $(TEST)/LexerTester.o
	rm -f $(TEST)/ParserTester.hi $(TEST)/ParserTester.o
	rm -f $(TEST)/SemanticTester.hi $(TEST)/SemanticTester.o 
	
