{
module AsmLexer where
import AsmToken
}

%wrapper "basic"

$digit =      [0-9]
$alpha =      [a-zA-Z]

tokens :-

    $white+                 ;
    \-?$digit+              { \s -> AsmTokenNum (read s) }
    r[1-9]$digit*           { \s -> AsmTokenReg (read (tail s)) }
    $alpha[$alpha$digit]*   { \s -> AsmTokenId s }
    \(                      { \s -> AsmTokenParenOpen }
    \)                      { \s -> AsmTokenParenClose }
    lc                      { \s -> AsmTokenLc }
    ld                      { \s -> AsmTokenLd }
    st                      { \s -> AsmTokenSt }
    add                     { \s -> AsmTokenAdd }
    sub                     { \s -> AsmTokenSub }
    mul                     { \s -> AsmTokenMul }
    div                     { \s -> AsmTokenDiv }
    lt                      { \s -> AsmTokenLt }
    gt                      { \s -> AsmTokenGt }
    eq                      { \s -> AsmTokenEq }
    br                      { \s -> AsmTokenBr }
    ret                     { \s -> AsmTokenRet }
    call                    { \s -> AsmTokenCall }
