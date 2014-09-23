LEXER=alex
PARSER=happy
HC=ghc
TEST=tests
VPATH = tests

all: misery

Lexer.hs: Misery.x
	$(LEXER) -o $@ $<

Parser.hs: Misery.y
	$(PARSER) -iParser.info -o $@ $<

misery: Misery.hs Lexer.hs Parser.hs Semantic.hs Generator.hs
	$(HC) --make -o $@ $<

AsmLexer.hs: Company.x
	$(LEXER) -o $@ $<

company: Company.hs AsmLexer.hs
	$(HC) --make -o $@ $<

tester: Tester.hs ParserTests.hs LexerTests.hs
	$(HC) --make -o $(TEST)/$@ $< -itests/

test: misery tester
	$(TEST)/tester

clean:
	rm -f Token.hi Token.o
	rm -f Program.hi Program.o
	rm -f Semantic.hi Semantic.o
	rm -f Generator.hi Generator.o
	rm -f Lexer.hs Lexer.hi Lexer.o
	rm -f Parser.info Parser.hs Parser.hi Parser.o
	rm -f Misery.hi Misery.o misery
	rm -f $(TEST)/Tester.hi $(TEST)/Tester.o $(TEST)/tester
	rm -f $(TEST)/LexerTests.hi $(TEST)/LexerTests.o
	rm -f $(TEST)/ParserTests.hi $(TEST)/ParserTests.o
	rm -f $(TEST)/SemanticTests.hi $(TEST)/SemanticTests.o
	rm -f AsmLexer.hs AsmLexer.hi
