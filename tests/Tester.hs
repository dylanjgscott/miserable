
-- | Tests
import ParserTests
import LexerTests
import InterpreterTests
import SemanticTests
import GeneratorTests
-- | Dependencies
import Parser
import Lexer
import Test.HUnit
import Token


main = 
    do
    putStrLn("Misery <3 Company will now run some tests.")

    putStrLn("------------------------------------------")

    putStrLn("Parser Tests: ")
    runTestTT parserTests

    putStrLn("------------------------------------------")

    putStrLn("Lexer tests: ")
    runTestTT lexerTests

    putStrLn("------------------------------------------")

    putStrLn("Semantic Tests: ")
    runTestTT semanticTests

    putStrLn("------------------------------------------")

    putStrLn("Interpreter Tests: ")
    runTestTT interpreterTests

    putStrLn("------------------------------------------")
    
    putStrLn("Generator Tests: ")
    runTestTT generatorTests

    putStrLn("------------------------------------------")

    putStrLn("Testing Complete.")



