LEXER=alex
PARSER=happy
HC=ghc
TEST=tests
VPATH = tests

all: Miserable tester

Lexer.hs: Miserable.x
	$(LEXER) -o $@ $<

Parser.hs: Miserable.y
	$(PARSER) -iParser.info -o $@ $<

tester: Tester.hs ParserTester.hs LexerTester.hs
	$(HC) --make -o $(TEST)/$@ $< -itests/

Miserable: Miserable.hs Lexer.hs Parser.hs Semantic.hs
	$(HC) --make -o $@ $<

clean:
	rm -f Token.hi Token.o
	rm -f Program.hi Program.o
	rm -f Semantic.hi Semantic.o
	rm -f Lexer.hs Lexer.hi Lexer.o
	rm -f Parser.info Parser.hs Parser.hi Parser.o
	rm -f Miserable.hi Miserable.o Miserable
	rm -f $(TEST)/Tester.hi $(TEST)/Tester.o $(TEST)/tester
	rm -f $(TEST)/LexerTester.hi $(TEST)/LexerTester.o
	rm -f $(TEST)/ParserTester.hi $(TEST)/ParserTester.o
	rm -f $(TEST)/SemanticTester.hi $(TEST)/SemanticTester.o 
	
