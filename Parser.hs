{-# OPTIONS_GHC -w #-}
module Parser where
import Program
import Token
import Data.Char

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

action_0 (16) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (16) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (16) = happyShift action_4
action_3 (5) = happyGoto action_8
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (30) = happyShift action_7
action_4 (14) = happyGoto action_6
action_4 _ = happyFail

action_5 (38) = happyAccept
action_5 _ = happyFail

action_6 (23) = happyShift action_10
action_6 (7) = happyGoto action_9
action_6 _ = happyFail

action_7 _ = happyReduce_28

action_8 _ = happyReduce_3

action_9 (17) = happyShift action_15
action_9 (8) = happyGoto action_14
action_9 _ = happyReduce_7

action_10 (24) = happyShift action_13
action_10 (30) = happyShift action_7
action_10 (9) = happyGoto action_11
action_10 (14) = happyGoto action_12
action_10 _ = happyFail

action_11 (24) = happyShift action_20
action_11 _ = happyFail

action_12 (19) = happyShift action_19
action_12 _ = happyReduce_9

action_13 _ = happyReduce_5

action_14 (20) = happyShift action_18
action_14 (10) = happyGoto action_17
action_14 _ = happyFail

action_15 (30) = happyShift action_7
action_15 (9) = happyGoto action_16
action_15 (14) = happyGoto action_12
action_15 _ = happyFail

action_16 (18) = happyShift action_27
action_16 _ = happyFail

action_17 _ = happyReduce_4

action_18 (25) = happyShift action_25
action_18 (28) = happyShift action_26
action_18 (30) = happyShift action_7
action_18 (11) = happyGoto action_22
action_18 (12) = happyGoto action_23
action_18 (14) = happyGoto action_24
action_18 _ = happyReduce_12

action_19 (30) = happyShift action_7
action_19 (9) = happyGoto action_21
action_19 (14) = happyGoto action_12
action_19 _ = happyFail

action_20 _ = happyReduce_6

action_21 _ = happyReduce_10

action_22 (21) = happyShift action_32
action_22 _ = happyFail

action_23 (18) = happyShift action_31
action_23 _ = happyFail

action_24 (22) = happyShift action_30
action_24 _ = happyFail

action_25 (30) = happyShift action_7
action_25 (14) = happyGoto action_29
action_25 _ = happyFail

action_26 (30) = happyShift action_7
action_26 (14) = happyGoto action_28
action_26 _ = happyFail

action_27 _ = happyReduce_8

action_28 _ = happyReduce_17

action_29 (26) = happyShift action_39
action_29 _ = happyFail

action_30 (23) = happyShift action_37
action_30 (29) = happyShift action_38
action_30 (30) = happyShift action_7
action_30 (13) = happyGoto action_34
action_30 (14) = happyGoto action_35
action_30 (15) = happyGoto action_36
action_30 _ = happyFail

action_31 (25) = happyShift action_25
action_31 (28) = happyShift action_26
action_31 (30) = happyShift action_7
action_31 (11) = happyGoto action_33
action_31 (12) = happyGoto action_23
action_31 (14) = happyGoto action_24
action_31 _ = happyReduce_12

action_32 _ = happyReduce_11

action_33 _ = happyReduce_13

action_34 _ = happyReduce_14

action_35 (23) = happyShift action_10
action_35 (7) = happyGoto action_42
action_35 _ = happyReduce_19

action_36 _ = happyReduce_18

action_37 (23) = happyShift action_37
action_37 (29) = happyShift action_38
action_37 (30) = happyShift action_7
action_37 (13) = happyGoto action_41
action_37 (14) = happyGoto action_35
action_37 (15) = happyGoto action_36
action_37 _ = happyFail

action_38 _ = happyReduce_29

action_39 (20) = happyShift action_18
action_39 (10) = happyGoto action_40
action_39 _ = happyFail

action_40 (27) = happyShift action_50
action_40 _ = happyReduce_15

action_41 (31) = happyShift action_43
action_41 (32) = happyShift action_44
action_41 (33) = happyShift action_45
action_41 (34) = happyShift action_46
action_41 (35) = happyShift action_47
action_41 (36) = happyShift action_48
action_41 (37) = happyShift action_49
action_41 _ = happyFail

action_42 _ = happyReduce_20

action_43 (23) = happyShift action_37
action_43 (29) = happyShift action_38
action_43 (30) = happyShift action_7
action_43 (13) = happyGoto action_58
action_43 (14) = happyGoto action_35
action_43 (15) = happyGoto action_36
action_43 _ = happyFail

action_44 (23) = happyShift action_37
action_44 (29) = happyShift action_38
action_44 (30) = happyShift action_7
action_44 (13) = happyGoto action_57
action_44 (14) = happyGoto action_35
action_44 (15) = happyGoto action_36
action_44 _ = happyFail

action_45 (23) = happyShift action_37
action_45 (29) = happyShift action_38
action_45 (30) = happyShift action_7
action_45 (13) = happyGoto action_56
action_45 (14) = happyGoto action_35
action_45 (15) = happyGoto action_36
action_45 _ = happyFail

action_46 (23) = happyShift action_37
action_46 (29) = happyShift action_38
action_46 (30) = happyShift action_7
action_46 (13) = happyGoto action_55
action_46 (14) = happyGoto action_35
action_46 (15) = happyGoto action_36
action_46 _ = happyFail

action_47 (23) = happyShift action_37
action_47 (29) = happyShift action_38
action_47 (30) = happyShift action_7
action_47 (13) = happyGoto action_54
action_47 (14) = happyGoto action_35
action_47 (15) = happyGoto action_36
action_47 _ = happyFail

action_48 (23) = happyShift action_37
action_48 (29) = happyShift action_38
action_48 (30) = happyShift action_7
action_48 (13) = happyGoto action_53
action_48 (14) = happyGoto action_35
action_48 (15) = happyGoto action_36
action_48 _ = happyFail

action_49 (23) = happyShift action_37
action_49 (29) = happyShift action_38
action_49 (30) = happyShift action_7
action_49 (13) = happyGoto action_52
action_49 (14) = happyGoto action_35
action_49 (15) = happyGoto action_36
action_49 _ = happyFail

action_50 (20) = happyShift action_18
action_50 (10) = happyGoto action_51
action_50 _ = happyFail

action_51 _ = happyReduce_16

action_52 (24) = happyShift action_65
action_52 _ = happyFail

action_53 (24) = happyShift action_64
action_53 _ = happyFail

action_54 (24) = happyShift action_63
action_54 _ = happyFail

action_55 (24) = happyShift action_62
action_55 _ = happyFail

action_56 (24) = happyShift action_61
action_56 _ = happyFail

action_57 (24) = happyShift action_60
action_57 _ = happyFail

action_58 (24) = happyShift action_59
action_58 _ = happyFail

action_59 _ = happyReduce_21

action_60 _ = happyReduce_22

action_61 _ = happyReduce_23

action_62 _ = happyReduce_24

action_63 _ = happyReduce_25

action_64 _ = happyReduce_26

action_65 _ = happyReduce_27

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Function happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	_
	 =  HappyAbsSyn7
		 (Args []
	)

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Args happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 (Vars []
	)

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Vars happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Block happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  11 happyReduction_12
happyReduction_12  =  HappyAbsSyn11
		 ([]
	)

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 12 happyReduction_15
happyReduction_15 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (IfElse happy_var_2 happy_var_4 (Block [])
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 12 happyReduction_16
happyReduction_16 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (IfElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Return happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn13
		 (ExpNum happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (ExpId happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  13 happyReduction_20
happyReduction_20 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (ExpFun happy_var_1 happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 5 13 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ExpOp OpAdd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 13 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ExpOp OpSub happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 13 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ExpOp OpMul happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 13 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ExpOp OpDiv happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 5 13 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ExpOp OpLT happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 13 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ExpOp OpGT happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 5 13 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ExpOp OpEq happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 (HappyTerminal (TokenId   happy_var_1))
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15 happyReduction_29
happyReduction_29 (HappyTerminal (TokenNum  happy_var_1))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 38 38 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenFunction -> cont 16;
	TokenVars -> cont 17;
	TokenSemicolon -> cont 18;
	TokenComma -> cont 19;
	TokenBegin -> cont 20;
	TokenEnd -> cont 21;
	TokenEquals -> cont 22;
	TokenParenOpen -> cont 23;
	TokenParenClose -> cont 24;
	TokenIf -> cont 25;
	TokenThen -> cont 26;
	TokenElse -> cont 27;
	TokenReturn -> cont 28;
	TokenNum  happy_dollar_dollar -> cont 29;
	TokenId   happy_dollar_dollar -> cont 30;
	TokenPlus -> cont 31;
	TokenMinus -> cont 32;
	TokenTimes -> cont 33;
	TokenDivide -> cont 34;
	TokenLT -> cont 35;
	TokenGT -> cont 36;
	TokenEQ -> cont 37;
	_ -> happyError' (tk:tks)
	}

happyError_ 38 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . syntaxError

calc tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Call this function when we get a parse error.
syntaxError :: [Token] -> a
syntaxError _ = error "Syntax Error."
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
