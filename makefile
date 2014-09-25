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

AsmParser.hs: Company.y
	$(PARSER) -iAsmParser.info -o $@ $<

company: Company.hs AsmLexer.hs AsmParser.hs Interpreter.hs
	$(HC) --make -o $@ $<

tester: Tester.hs ParserTests.hs LexerTests.hs InterpreterTests.hs
	$(HC) --make -o $(TEST)/$@ $< -itests/

test: misery tester
	$(TEST)/tester

clean:
	rm -f Token.hi Token.o
	rm -f Program.hi Program.o
	rm -f Semantic.hi Semantic.o
	rm -f Generator.hi Generator.o
	rm -f Interpreter.hi Interpreter.o
	rm -f Lexer.hs Lexer.hi Lexer.o
	rm -f Parser.info Parser.hs Parser.hi Parser.o
	rm -f Misery.hi Misery.o misery
	rm -f $(TEST)/Tester.hi $(TEST)/Tester.o $(TEST)/tester
	rm -f $(TEST)/LexerTests.hi $(TEST)/LexerTests.o
	rm -f $(TEST)/ParserTests.hi $(TEST)/ParserTests.o
	rm -f $(TEST)/SemanticTests.hi $(TEST)/SemanticTests.o
	rm -f AsmToken.hi AsmToken.o
	rm -f Assembly.hi Assembly.o
	rm -f Interpreter.hi Interpreter.o
	rm -f AsmLexer.hs AsmLexer.hi AsmLexer.o
	rm -f AsmParser.info AsmParser.hs AsmParser.hi AsmParser.o
	rm -f Company.hi Company.o company
