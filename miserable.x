{
module Lexer where
import Token
}

%wrapper "basic"

$digit =      [0-9]
$alpha =      [a-zA-Z]

tokens :-

    $white+               ;
    FUNCTION              { \s -> Function }
    VARS                  { \s -> Vars }
    \;                    { \s -> Semicolon }
    \,                    { \s -> Comma }
    BEGIN                 { \s -> Begin }
    END                   { \s -> End }
    =                     { \s -> Equals }
    \(                    { \s -> ParenOpen }
    \)                    { \s -> ParenClose }
    IF                    { \s -> If }
    THEN                  { \s -> Then }
    ELSE                  { \s -> Else }
    RETURN                { \s -> Return }
    \-?$digit+ 	          { \s -> Num (read s) }
    $alpha[$alpha$digit]* { \s -> Id s }
    \+                    { \s -> Op s }
    \-                     { \s -> Op s }
    \*                    { \s -> Op s }
    \/                     { \s -> Op s }
    \<                     { \s -> Op s }
    \>                     { \s -> Op s }
    ==                    { \s -> Op s }
