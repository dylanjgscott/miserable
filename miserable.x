%wrapper "basic"

$digit =      [0-9]
$alpha =      [a-zA-z]

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

-- I believe these have to go last otherwise 'VARS' becomes
-- an 'Id' token instead of a 'Vars' token.
    \-?$digit+ 	          { \s -> Num (read s) }
    $alpha[$alpha$digit]* { \s -> Id s }
    \+                    { \s -> Op s }
    \-                     { \s -> Op s }
    \*                    { \s -> Op s }
    \/                     { \s -> Op s }
    \<                     { \s -> Op s }
    \>                     { \s -> Op s }
    ==                    { \s -> Op s }

{
data Token = Function
           | Vars
           | Semicolon
           | Comma
           | Begin
           | End
           | Equals
           | ParenOpen
           | ParenClose
           | If
           | Then
           | Else
           | Return
           | Num Int
           | Id String
           | Op String
           deriving (Eq, Show)

main = do
    s <- getContents
    print $ alexScanTokens s
}
