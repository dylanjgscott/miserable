module AsmToken where

data AsmToken = AsmTokenNum AsmNum
              | AsmTokenId AsmId
              | AsmTokenParenOpen
              | AsmTokenParenClose
              | AsmTokenLc
              | AsmTokenLd
              | AsmTokenSt
              | AsmTokenAdd
              | AsmTokenSub
              | AsmTokenMul
              | AsmTokenDiv
              | AsmTokenLt
              | AsmTokenGt
              | AsmTokenEq
              | AsmTokenBr
              | AsmTokenRet
              | AsmTokenCall
              deriving (Show, Eq)
          
type AsmNum = Integer

type AsmId = String

type AsmReg = Integer
