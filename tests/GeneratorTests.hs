module GeneratorTests where
-- Misery & Company
import Lexer
import Parser
import Semantic
import TestLib
-- Others
import Test.HUnit
import Control.Exception
import Control.Monad


-- | Define tests


--todo

test0 = TestCase (do
        assertEqual "I am a test" True True)




-- | pass to tester
generatorTests = TestList   [TestLabel " Dummy Test " test0]


