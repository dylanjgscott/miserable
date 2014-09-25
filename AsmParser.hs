{-# OPTIONS_GHC -w #-}
module AsmParser where
import Assembly
import AsmToken

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
	= HappyTerminal (AsmToken)
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

action_0 (19) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (19) = happyShift action_2
action_1 _ = happyFail

action_2 (19) = happyShift action_6
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 _ = happyReduce_2

action_3 (34) = happyAccept
action_3 _ = happyFail

action_4 (20) = happyShift action_10
action_4 _ = happyFail

action_5 (19) = happyShift action_6
action_5 (5) = happyGoto action_9
action_5 (6) = happyGoto action_5
action_5 _ = happyReduce_2

action_6 (18) = happyShift action_8
action_6 (15) = happyGoto action_7
action_6 _ = happyFail

action_7 (19) = happyShift action_12
action_7 (7) = happyGoto action_11
action_7 _ = happyFail

action_8 _ = happyReduce_29

action_9 _ = happyReduce_3

action_10 _ = happyReduce_1

action_11 (19) = happyShift action_16
action_11 (9) = happyGoto action_15
action_11 _ = happyFail

action_12 (18) = happyShift action_8
action_12 (8) = happyGoto action_13
action_12 (15) = happyGoto action_14
action_12 _ = happyReduce_6

action_13 (20) = happyShift action_21
action_13 _ = happyFail

action_14 (18) = happyShift action_8
action_14 (8) = happyGoto action_20
action_14 (15) = happyGoto action_14
action_14 _ = happyReduce_6

action_15 (20) = happyShift action_19
action_15 _ = happyFail

action_16 (16) = happyShift action_18
action_16 (13) = happyGoto action_17
action_16 _ = happyFail

action_17 (19) = happyShift action_24
action_17 (10) = happyGoto action_22
action_17 (11) = happyGoto action_23
action_17 _ = happyFail

action_18 _ = happyReduce_27

action_19 _ = happyReduce_4

action_20 _ = happyReduce_7

action_21 _ = happyReduce_5

action_22 (20) = happyShift action_39
action_22 _ = happyFail

action_23 (19) = happyShift action_24
action_23 (10) = happyGoto action_38
action_23 (11) = happyGoto action_23
action_23 _ = happyReduce_10

action_24 (21) = happyShift action_25
action_24 (22) = happyShift action_26
action_24 (23) = happyShift action_27
action_24 (24) = happyShift action_28
action_24 (25) = happyShift action_29
action_24 (26) = happyShift action_30
action_24 (27) = happyShift action_31
action_24 (28) = happyShift action_32
action_24 (29) = happyShift action_33
action_24 (30) = happyShift action_34
action_24 (31) = happyShift action_35
action_24 (32) = happyShift action_36
action_24 (33) = happyShift action_37
action_24 _ = happyFail

action_25 (17) = happyShift action_42
action_25 (14) = happyGoto action_54
action_25 _ = happyFail

action_26 (17) = happyShift action_42
action_26 (14) = happyGoto action_53
action_26 _ = happyFail

action_27 (18) = happyShift action_8
action_27 (15) = happyGoto action_52
action_27 _ = happyFail

action_28 (17) = happyShift action_42
action_28 (14) = happyGoto action_51
action_28 _ = happyFail

action_29 (17) = happyShift action_42
action_29 (14) = happyGoto action_50
action_29 _ = happyFail

action_30 (17) = happyShift action_42
action_30 (14) = happyGoto action_49
action_30 _ = happyFail

action_31 (17) = happyShift action_42
action_31 (14) = happyGoto action_48
action_31 _ = happyFail

action_32 (17) = happyShift action_42
action_32 (14) = happyGoto action_47
action_32 _ = happyFail

action_33 (17) = happyShift action_42
action_33 (14) = happyGoto action_46
action_33 _ = happyFail

action_34 (17) = happyShift action_42
action_34 (14) = happyGoto action_45
action_34 _ = happyFail

action_35 (17) = happyShift action_42
action_35 (14) = happyGoto action_44
action_35 _ = happyFail

action_36 (17) = happyShift action_42
action_36 (14) = happyGoto action_43
action_36 _ = happyFail

action_37 (17) = happyShift action_42
action_37 (14) = happyGoto action_41
action_37 _ = happyFail

action_38 _ = happyReduce_11

action_39 (19) = happyShift action_16
action_39 (9) = happyGoto action_40
action_39 _ = happyReduce_8

action_40 _ = happyReduce_9

action_41 (18) = happyShift action_8
action_41 (15) = happyGoto action_67
action_41 _ = happyFail

action_42 _ = happyReduce_28

action_43 (20) = happyShift action_66
action_43 _ = happyFail

action_44 (16) = happyShift action_18
action_44 (13) = happyGoto action_65
action_44 _ = happyFail

action_45 (17) = happyShift action_42
action_45 (14) = happyGoto action_64
action_45 _ = happyFail

action_46 (17) = happyShift action_42
action_46 (14) = happyGoto action_63
action_46 _ = happyFail

action_47 (17) = happyShift action_42
action_47 (14) = happyGoto action_62
action_47 _ = happyFail

action_48 (17) = happyShift action_42
action_48 (14) = happyGoto action_61
action_48 _ = happyFail

action_49 (17) = happyShift action_42
action_49 (14) = happyGoto action_60
action_49 _ = happyFail

action_50 (17) = happyShift action_42
action_50 (14) = happyGoto action_59
action_50 _ = happyFail

action_51 (17) = happyShift action_42
action_51 (14) = happyGoto action_58
action_51 _ = happyFail

action_52 (17) = happyShift action_42
action_52 (14) = happyGoto action_57
action_52 _ = happyFail

action_53 (18) = happyShift action_8
action_53 (15) = happyGoto action_56
action_53 _ = happyFail

action_54 (16) = happyShift action_18
action_54 (13) = happyGoto action_55
action_54 _ = happyFail

action_55 (20) = happyShift action_80
action_55 _ = happyFail

action_56 (20) = happyShift action_79
action_56 _ = happyFail

action_57 (20) = happyShift action_78
action_57 _ = happyFail

action_58 (17) = happyShift action_42
action_58 (14) = happyGoto action_77
action_58 _ = happyFail

action_59 (17) = happyShift action_42
action_59 (14) = happyGoto action_76
action_59 _ = happyFail

action_60 (17) = happyShift action_42
action_60 (14) = happyGoto action_75
action_60 _ = happyFail

action_61 (17) = happyShift action_42
action_61 (14) = happyGoto action_74
action_61 _ = happyFail

action_62 (17) = happyShift action_42
action_62 (14) = happyGoto action_73
action_62 _ = happyFail

action_63 (17) = happyShift action_42
action_63 (14) = happyGoto action_72
action_63 _ = happyFail

action_64 (17) = happyShift action_42
action_64 (14) = happyGoto action_71
action_64 _ = happyFail

action_65 (16) = happyShift action_18
action_65 (13) = happyGoto action_70
action_65 _ = happyFail

action_66 _ = happyReduce_23

action_67 (17) = happyShift action_42
action_67 (12) = happyGoto action_68
action_67 (14) = happyGoto action_69
action_67 _ = happyReduce_25

action_68 (20) = happyShift action_90
action_68 _ = happyFail

action_69 (17) = happyShift action_42
action_69 (12) = happyGoto action_89
action_69 (14) = happyGoto action_69
action_69 _ = happyReduce_25

action_70 (20) = happyShift action_88
action_70 _ = happyFail

action_71 (20) = happyShift action_87
action_71 _ = happyFail

action_72 (20) = happyShift action_86
action_72 _ = happyFail

action_73 (20) = happyShift action_85
action_73 _ = happyFail

action_74 (20) = happyShift action_84
action_74 _ = happyFail

action_75 (20) = happyShift action_83
action_75 _ = happyFail

action_76 (20) = happyShift action_82
action_76 _ = happyFail

action_77 (20) = happyShift action_81
action_77 _ = happyFail

action_78 _ = happyReduce_14

action_79 _ = happyReduce_13

action_80 _ = happyReduce_12

action_81 _ = happyReduce_15

action_82 _ = happyReduce_16

action_83 _ = happyReduce_17

action_84 _ = happyReduce_18

action_85 _ = happyReduce_19

action_86 _ = happyReduce_20

action_87 _ = happyReduce_21

action_88 _ = happyReduce_22

action_89 _ = happyReduce_26

action_90 _ = happyReduce_24

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

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
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AsmFunction happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  8 happyReduction_6
happyReduction_6  =  HappyAbsSyn8
		 ([]
	)

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 9 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (AsmBlock happy_var_2 happy_var_3 : []
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 5 9 happyReduction_9
happyReduction_9 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (AsmBlock happy_var_2 happy_var_3 : happy_var_5
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : []
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 5 11 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmLc happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 5 11 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmLd happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 11 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmSt happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 6 11 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmAdd happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 11 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmSub happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 6 11 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmMul happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 11 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmDiv happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 11 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmLt happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 6 11 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmGt happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 11 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmEq happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 6 11 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmBr happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 11 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmRet happy_var_3
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 6 11 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (AsmCall happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_0  12 happyReduction_25
happyReduction_25  =  HappyAbsSyn12
		 ([]
	)

happyReduce_26 = happySpecReduce_2  12 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  13 happyReduction_27
happyReduction_27 (HappyTerminal (AsmTokenNum   happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 (HappyTerminal (AsmTokenReg   happy_var_1))
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15 happyReduction_29
happyReduction_29 (HappyTerminal (AsmTokenId    happy_var_1))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	AsmTokenNum   happy_dollar_dollar -> cont 16;
	AsmTokenReg   happy_dollar_dollar -> cont 17;
	AsmTokenId    happy_dollar_dollar -> cont 18;
	AsmTokenParenOpen -> cont 19;
	AsmTokenParenClose -> cont 20;
	AsmTokenLc -> cont 21;
	AsmTokenLd -> cont 22;
	AsmTokenSt -> cont 23;
	AsmTokenAdd -> cont 24;
	AsmTokenSub -> cont 25;
	AsmTokenMul -> cont 26;
	AsmTokenDiv -> cont 27;
	AsmTokenLt -> cont 28;
	AsmTokenGt -> cont 29;
	AsmTokenEq -> cont 30;
	AsmTokenBr -> cont 31;
	AsmTokenRet -> cont 32;
	AsmTokenCall -> cont 33;
	_ -> happyError' (tk:tks)
	}

happyError_ 34 tk tks = happyError' tks
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
happyError' :: () => [(AsmToken)] -> HappyIdentity a
happyError' = HappyIdentity . syntaxError

asmParse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Call this function when we get a parse error.
syntaxError :: [AsmToken] -> a
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
