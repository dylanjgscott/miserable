{
module Lexer where
import Token
}

%wrapper "basic"

$digit =      [0-9]
$alpha =      [a-zA-Z]

tokens :-

    $white+               ;
    FUNCTION              { \s -> TokenFunction }
    VARS                  { \s -> TokenVars }
    \;                    { \s -> TokenSemicolon }
    \,                    { \s -> TokenComma }
    BEGIN                 { \s -> TokenBegin }
    END                   { \s -> TokenEnd }
    =                     { \s -> TokenEquals }
    \(                    { \s -> TokenParenOpen }
    \)                    { \s -> TokenParenClose }
    IF                    { \s -> TokenIf }
    THEN                  { \s -> TokenThen }
    ELSE                  { \s -> TokenElse }
    RETURN                { \s -> TokenReturn }
    \-?$digit+ 	          { \s -> TokenNum (read s) }
    $alpha[$alpha$digit]* { \s -> TokenId s }
    \+                    { \s -> TokenPlus}
    \-                     { \s -> TokenMinus}
    \*                    { \s -> TokenTimes}
    \/                     { \s -> TokenDivide}
    \<                     { \s -> TokenLT}
    \>                     { \s -> TokenGT}
    ==                    { \s -> TokenEQ}
