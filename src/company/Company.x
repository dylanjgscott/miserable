{
module AsmLexer where
import AsmToken
}

%wrapper "basic"

$digit =      [0-9]
$alpha =      [a-zA-Z]

tokens :-

    $white+                 ;
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
    cmp                     { \s -> AsmTokenEq }
    br                      { \s -> AsmTokenBr }
    ret                     { \s -> AsmTokenRet }
    call                    { \s -> AsmTokenCall }
    \-?$digit+              { \s -> AsmTokenNum (read s) }
    $alpha[$alpha$digit]*   { \s -> AsmTokenId s }
    \(                      { \s -> AsmTokenParenOpen }
    \)                      { \s -> AsmTokenParenClose }
