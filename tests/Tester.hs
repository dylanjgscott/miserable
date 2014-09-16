import ParserTests
import LexerTests
import Parser
import Lexer
import Test.HUnit


main = 
	do
	print("Parser Tests: ")
	runTestTT parserTests
		
	print("Lexer tests: ")
	runTestTT lexerTests

