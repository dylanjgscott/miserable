
-- | Tests
import ParserTests
import LexerTests
import InterpreterTests
import SemanticTests

-- | Dependencies
import Parser
import Lexer
import Test.HUnit
import Token


main = 
	do
	putStr("Misery <3 Company will now run some tests.\n")

	putStr("------------------------------------------\n")

	putStr("Parser Tests: \n")
	runTestTT parserTests
		
	putStr("------------------------------------------\n")

	putStr("Lexer tests: \n")
	runTestTT lexerTests

	putStr("------------------------------------------\n")

	putStr("Semantic Tests: \n")
	runTestTT semanticTests

	putStr("------------------------------------------\n")

	putStr("Interpreter Tests: \n")
	runTestTT interpreterTests

	print("Testing Complete.")



