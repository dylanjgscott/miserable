import ParserTests
import LexerTests
import InterpreterTests
import Parser
import Lexer
import Test.HUnit
import Token


main = 
	do
	print("Parser Tests: ")
	runTestTT parserTests
		
	print("Lexer tests: ")
	runTestTT lexerTests

	print("Semantic Tests: ")
	--runTestTT semanticTests

	print("Interpreter Tests: ")
	runTestTT interpreterTests



