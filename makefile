LEXER=alex
PARSER=happy
HC=ghc
TEST_SRC=tests
MISERY_SRC=src/misery
COMPANY_SRC=src/company
VPATH=$(TEST_SRC)

all: misery company

misery:
	cd $(MISERY_SRC); make

company:
	cd $(COMPANY_SRC); make

tester: Tester.hs misery company ParserTests.hs LexerTests.hs InterpreterTests.hs TestLib.hs GeneratorTests.hs
	$(HC) --make -i$(MISERY_SRC):$(COMPANY_SRC):$(TEST_SRC) -o $@ $<

test: tester
	./tester

clean:
	cd $(MISERY_SRC); make clean
	cd $(COMPANY_SRC); make clean
	rm -f $(TEST_SRC)/Tester.hi $(TEST_SRC)/Tester.o tester
	rm -f $(TEST_SRC)/LexerTests.hi $(TEST_SRC)/LexerTests.o
	rm -f $(TEST_SRC)/ParserTests.hi $(TEST_SRC)/ParserTests.o
	rm -f $(TEST_SRC)/SemanticTests.hi $(TEST_SRC)/SemanticTests.o
	rm -f $(TEST_SRC)/GeneratorTests.hi $(TEST_SRC)/GeneratorTests.o
	rm -f $(TEST_SRC)/InterpreterTests.hi $(TEST_SRC)/InterpreterTests.o
	rm -f $(TEST_SRC)/TestLib.hi $(TEST_SRC)/TestLib.o
