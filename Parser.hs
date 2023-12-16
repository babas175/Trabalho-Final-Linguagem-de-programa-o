{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,304) ([26400,49161,128,0,0,0,63488,49753,61,0,0,0,0,38514,3072,0,0,8192,0,40064,37,3,2,51200,601,48,256,32768,9628,768,0,0,0,1,26592,63243,0,512,65024,28822,15,0,8192,2407,32960,9628,768,38514,3072,22984,12290,26400,49161,40064,37,29187,150,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29184,150,12,6208,0,0,32768,9628,768,38526,8048,22984,12290,26592,63257,0,2,0,1552,0,0,0,0,32768,9695,988,38514,3072,8192,0,26400,49161,40064,37,32259,28822,51231,601,48,0,0,0,0,4096,6,0,0,0,2,40832,56365,29187,150,12,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","Type","num","'+'","\"&&\"","true","false","if","then","else","var","'\\\\'","\"->\"","'('","')'","'='","let","in","Bool","Num","':'","'*'","'-'","'|'","\"==\"","\">\"","\">=\"","'!'","for","';'","%eof"]
        bit_start = st * 34
        bit_end = (st + 1) * 34
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..33]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_2
action_0 (9) = happyShift action_4
action_0 (10) = happyShift action_5
action_0 (11) = happyShift action_6
action_0 (14) = happyShift action_7
action_0 (15) = happyShift action_8
action_0 (17) = happyShift action_9
action_0 (20) = happyShift action_10
action_0 (31) = happyShift action_11
action_0 (32) = happyShift action_12
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (6) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (6) = happyShift action_2
action_3 (7) = happyShift action_20
action_3 (8) = happyShift action_21
action_3 (9) = happyShift action_4
action_3 (10) = happyShift action_5
action_3 (11) = happyShift action_6
action_3 (14) = happyShift action_7
action_3 (15) = happyShift action_8
action_3 (17) = happyShift action_9
action_3 (20) = happyShift action_10
action_3 (25) = happyShift action_22
action_3 (26) = happyShift action_23
action_3 (27) = happyShift action_24
action_3 (29) = happyShift action_25
action_3 (30) = happyShift action_26
action_3 (31) = happyShift action_11
action_3 (32) = happyShift action_12
action_3 (34) = happyAccept
action_3 (4) = happyGoto action_19
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 _ = happyReduce_3

action_6 (6) = happyShift action_2
action_6 (9) = happyShift action_4
action_6 (10) = happyShift action_5
action_6 (11) = happyShift action_6
action_6 (14) = happyShift action_7
action_6 (15) = happyShift action_8
action_6 (17) = happyShift action_9
action_6 (20) = happyShift action_10
action_6 (31) = happyShift action_11
action_6 (32) = happyShift action_12
action_6 (4) = happyGoto action_18
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_7

action_8 (14) = happyShift action_17
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (6) = happyShift action_2
action_9 (9) = happyShift action_4
action_9 (10) = happyShift action_5
action_9 (11) = happyShift action_6
action_9 (14) = happyShift action_7
action_9 (15) = happyShift action_8
action_9 (17) = happyShift action_9
action_9 (20) = happyShift action_10
action_9 (31) = happyShift action_11
action_9 (32) = happyShift action_12
action_9 (4) = happyGoto action_16
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (14) = happyShift action_15
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (6) = happyShift action_2
action_11 (9) = happyShift action_4
action_11 (10) = happyShift action_5
action_11 (11) = happyShift action_6
action_11 (14) = happyShift action_7
action_11 (15) = happyShift action_8
action_11 (17) = happyShift action_9
action_11 (20) = happyShift action_10
action_11 (31) = happyShift action_11
action_11 (32) = happyShift action_12
action_11 (4) = happyGoto action_14
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (17) = happyShift action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (6) = happyShift action_2
action_13 (9) = happyShift action_4
action_13 (10) = happyShift action_5
action_13 (11) = happyShift action_6
action_13 (14) = happyShift action_7
action_13 (15) = happyShift action_8
action_13 (17) = happyShift action_9
action_13 (20) = happyShift action_10
action_13 (31) = happyShift action_11
action_13 (32) = happyShift action_12
action_13 (4) = happyGoto action_38
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (6) = happyShift action_2
action_14 (7) = happyShift action_20
action_14 (8) = happyShift action_21
action_14 (9) = happyShift action_4
action_14 (10) = happyShift action_5
action_14 (11) = happyShift action_6
action_14 (14) = happyShift action_7
action_14 (15) = happyShift action_8
action_14 (17) = happyShift action_9
action_14 (20) = happyShift action_10
action_14 (25) = happyShift action_22
action_14 (26) = happyShift action_23
action_14 (27) = happyShift action_24
action_14 (29) = happyShift action_25
action_14 (30) = happyShift action_26
action_14 (31) = happyShift action_11
action_14 (32) = happyShift action_12
action_14 (4) = happyGoto action_19
action_14 _ = happyReduce_17

action_15 (19) = happyShift action_37
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (6) = happyShift action_2
action_16 (7) = happyShift action_20
action_16 (8) = happyShift action_21
action_16 (9) = happyShift action_4
action_16 (10) = happyShift action_5
action_16 (11) = happyShift action_6
action_16 (14) = happyShift action_7
action_16 (15) = happyShift action_8
action_16 (17) = happyShift action_9
action_16 (18) = happyShift action_36
action_16 (20) = happyShift action_10
action_16 (25) = happyShift action_22
action_16 (26) = happyShift action_23
action_16 (27) = happyShift action_24
action_16 (29) = happyShift action_25
action_16 (30) = happyShift action_26
action_16 (31) = happyShift action_11
action_16 (32) = happyShift action_12
action_16 (4) = happyGoto action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (24) = happyShift action_35
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (6) = happyShift action_2
action_18 (7) = happyShift action_20
action_18 (8) = happyShift action_21
action_18 (9) = happyShift action_4
action_18 (10) = happyShift action_5
action_18 (11) = happyShift action_6
action_18 (12) = happyShift action_34
action_18 (14) = happyShift action_7
action_18 (15) = happyShift action_8
action_18 (17) = happyShift action_9
action_18 (20) = happyShift action_10
action_18 (25) = happyShift action_22
action_18 (26) = happyShift action_23
action_18 (27) = happyShift action_24
action_18 (29) = happyShift action_25
action_18 (30) = happyShift action_26
action_18 (31) = happyShift action_11
action_18 (32) = happyShift action_12
action_18 (4) = happyGoto action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (6) = happyShift action_2
action_19 (7) = happyShift action_20
action_19 (8) = happyShift action_21
action_19 (9) = happyShift action_4
action_19 (10) = happyShift action_5
action_19 (11) = happyShift action_6
action_19 (14) = happyShift action_7
action_19 (15) = happyShift action_8
action_19 (17) = happyShift action_9
action_19 (20) = happyShift action_10
action_19 (25) = happyShift action_22
action_19 (26) = happyShift action_23
action_19 (27) = happyShift action_24
action_19 (29) = happyShift action_25
action_19 (30) = happyShift action_26
action_19 (31) = happyShift action_11
action_19 (32) = happyShift action_12
action_19 (4) = happyGoto action_19
action_19 _ = happyReduce_9

action_20 (6) = happyShift action_2
action_20 (9) = happyShift action_4
action_20 (10) = happyShift action_5
action_20 (11) = happyShift action_6
action_20 (14) = happyShift action_7
action_20 (15) = happyShift action_8
action_20 (17) = happyShift action_9
action_20 (20) = happyShift action_10
action_20 (31) = happyShift action_11
action_20 (32) = happyShift action_12
action_20 (4) = happyGoto action_33
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (6) = happyShift action_2
action_21 (9) = happyShift action_4
action_21 (10) = happyShift action_5
action_21 (11) = happyShift action_6
action_21 (14) = happyShift action_7
action_21 (15) = happyShift action_8
action_21 (17) = happyShift action_9
action_21 (20) = happyShift action_10
action_21 (31) = happyShift action_11
action_21 (32) = happyShift action_12
action_21 (4) = happyGoto action_32
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (6) = happyShift action_2
action_22 (9) = happyShift action_4
action_22 (10) = happyShift action_5
action_22 (11) = happyShift action_6
action_22 (14) = happyShift action_7
action_22 (15) = happyShift action_8
action_22 (17) = happyShift action_9
action_22 (20) = happyShift action_10
action_22 (31) = happyShift action_11
action_22 (32) = happyShift action_12
action_22 (4) = happyGoto action_31
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (6) = happyShift action_2
action_23 (9) = happyShift action_4
action_23 (10) = happyShift action_5
action_23 (11) = happyShift action_6
action_23 (14) = happyShift action_7
action_23 (15) = happyShift action_8
action_23 (17) = happyShift action_9
action_23 (20) = happyShift action_10
action_23 (31) = happyShift action_11
action_23 (32) = happyShift action_12
action_23 (4) = happyGoto action_30
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (6) = happyShift action_2
action_24 (9) = happyShift action_4
action_24 (10) = happyShift action_5
action_24 (11) = happyShift action_6
action_24 (14) = happyShift action_7
action_24 (15) = happyShift action_8
action_24 (17) = happyShift action_9
action_24 (20) = happyShift action_10
action_24 (31) = happyShift action_11
action_24 (32) = happyShift action_12
action_24 (4) = happyGoto action_29
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (6) = happyShift action_2
action_25 (9) = happyShift action_4
action_25 (10) = happyShift action_5
action_25 (11) = happyShift action_6
action_25 (14) = happyShift action_7
action_25 (15) = happyShift action_8
action_25 (17) = happyShift action_9
action_25 (20) = happyShift action_10
action_25 (31) = happyShift action_11
action_25 (32) = happyShift action_12
action_25 (4) = happyGoto action_28
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (6) = happyShift action_2
action_26 (9) = happyShift action_4
action_26 (10) = happyShift action_5
action_26 (11) = happyShift action_6
action_26 (14) = happyShift action_7
action_26 (15) = happyShift action_8
action_26 (17) = happyShift action_9
action_26 (20) = happyShift action_10
action_26 (31) = happyShift action_11
action_26 (32) = happyShift action_12
action_26 (4) = happyGoto action_27
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (6) = happyShift action_2
action_27 (7) = happyShift action_20
action_27 (8) = happyShift action_21
action_27 (9) = happyShift action_4
action_27 (10) = happyShift action_5
action_27 (11) = happyShift action_6
action_27 (14) = happyShift action_7
action_27 (15) = happyShift action_8
action_27 (17) = happyShift action_9
action_27 (20) = happyShift action_10
action_27 (25) = happyShift action_22
action_27 (26) = happyShift action_23
action_27 (27) = happyShift action_24
action_27 (29) = happyShift action_25
action_27 (30) = happyShift action_26
action_27 (31) = happyShift action_11
action_27 (32) = happyShift action_12
action_27 (4) = happyGoto action_19
action_27 _ = happyReduce_16

action_28 (6) = happyShift action_2
action_28 (7) = happyShift action_20
action_28 (8) = happyShift action_21
action_28 (9) = happyShift action_4
action_28 (10) = happyShift action_5
action_28 (11) = happyShift action_6
action_28 (14) = happyShift action_7
action_28 (15) = happyShift action_8
action_28 (17) = happyShift action_9
action_28 (20) = happyShift action_10
action_28 (25) = happyShift action_22
action_28 (26) = happyShift action_23
action_28 (27) = happyShift action_24
action_28 (29) = happyShift action_25
action_28 (30) = happyShift action_26
action_28 (31) = happyShift action_11
action_28 (32) = happyShift action_12
action_28 (4) = happyGoto action_19
action_28 _ = happyReduce_15

action_29 (6) = happyShift action_2
action_29 (7) = happyShift action_20
action_29 (8) = happyShift action_21
action_29 (9) = happyShift action_4
action_29 (10) = happyShift action_5
action_29 (11) = happyShift action_6
action_29 (14) = happyShift action_7
action_29 (15) = happyShift action_8
action_29 (17) = happyShift action_9
action_29 (20) = happyShift action_10
action_29 (25) = happyShift action_22
action_29 (26) = happyShift action_23
action_29 (27) = happyShift action_24
action_29 (29) = happyShift action_25
action_29 (30) = happyShift action_26
action_29 (31) = happyShift action_11
action_29 (32) = happyShift action_12
action_29 (4) = happyGoto action_19
action_29 _ = happyReduce_14

action_30 (6) = happyShift action_2
action_30 (7) = happyShift action_20
action_30 (8) = happyShift action_21
action_30 (9) = happyShift action_4
action_30 (10) = happyShift action_5
action_30 (11) = happyShift action_6
action_30 (14) = happyShift action_7
action_30 (15) = happyShift action_8
action_30 (17) = happyShift action_9
action_30 (20) = happyShift action_10
action_30 (25) = happyShift action_22
action_30 (26) = happyShift action_23
action_30 (27) = happyShift action_24
action_30 (29) = happyShift action_25
action_30 (30) = happyShift action_26
action_30 (31) = happyShift action_11
action_30 (32) = happyShift action_12
action_30 (4) = happyGoto action_19
action_30 _ = happyReduce_13

action_31 (6) = happyShift action_2
action_31 (7) = happyShift action_20
action_31 (8) = happyShift action_21
action_31 (9) = happyShift action_4
action_31 (10) = happyShift action_5
action_31 (11) = happyShift action_6
action_31 (14) = happyShift action_7
action_31 (15) = happyShift action_8
action_31 (17) = happyShift action_9
action_31 (20) = happyShift action_10
action_31 (25) = happyShift action_22
action_31 (26) = happyShift action_23
action_31 (27) = happyShift action_24
action_31 (29) = happyShift action_25
action_31 (30) = happyShift action_26
action_31 (31) = happyShift action_11
action_31 (32) = happyShift action_12
action_31 (4) = happyGoto action_19
action_31 _ = happyReduce_12

action_32 (6) = happyShift action_2
action_32 (7) = happyShift action_20
action_32 (8) = happyShift action_21
action_32 (9) = happyShift action_4
action_32 (10) = happyShift action_5
action_32 (11) = happyShift action_6
action_32 (14) = happyShift action_7
action_32 (15) = happyShift action_8
action_32 (17) = happyShift action_9
action_32 (20) = happyShift action_10
action_32 (25) = happyShift action_22
action_32 (26) = happyShift action_23
action_32 (27) = happyShift action_24
action_32 (29) = happyShift action_25
action_32 (30) = happyShift action_26
action_32 (31) = happyShift action_11
action_32 (32) = happyShift action_12
action_32 (4) = happyGoto action_19
action_32 _ = happyReduce_5

action_33 (6) = happyShift action_2
action_33 (8) = happyShift action_21
action_33 (9) = happyShift action_4
action_33 (10) = happyShift action_5
action_33 (11) = happyShift action_6
action_33 (14) = happyShift action_7
action_33 (15) = happyShift action_8
action_33 (17) = happyShift action_9
action_33 (20) = happyShift action_10
action_33 (25) = happyShift action_22
action_33 (26) = happyShift action_23
action_33 (27) = happyShift action_24
action_33 (29) = happyShift action_25
action_33 (30) = happyShift action_26
action_33 (31) = happyShift action_11
action_33 (32) = happyShift action_12
action_33 (4) = happyGoto action_19
action_33 _ = happyReduce_4

action_34 (6) = happyShift action_2
action_34 (9) = happyShift action_4
action_34 (10) = happyShift action_5
action_34 (11) = happyShift action_6
action_34 (14) = happyShift action_7
action_34 (15) = happyShift action_8
action_34 (17) = happyShift action_9
action_34 (20) = happyShift action_10
action_34 (31) = happyShift action_11
action_34 (32) = happyShift action_12
action_34 (4) = happyGoto action_45
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (17) = happyShift action_42
action_35 (22) = happyShift action_43
action_35 (23) = happyShift action_44
action_35 (5) = happyGoto action_41
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_10

action_37 (6) = happyShift action_2
action_37 (9) = happyShift action_4
action_37 (10) = happyShift action_5
action_37 (11) = happyShift action_6
action_37 (14) = happyShift action_7
action_37 (15) = happyShift action_8
action_37 (17) = happyShift action_9
action_37 (20) = happyShift action_10
action_37 (31) = happyShift action_11
action_37 (32) = happyShift action_12
action_37 (4) = happyGoto action_40
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (6) = happyShift action_2
action_38 (7) = happyShift action_20
action_38 (8) = happyShift action_21
action_38 (9) = happyShift action_4
action_38 (10) = happyShift action_5
action_38 (11) = happyShift action_6
action_38 (14) = happyShift action_7
action_38 (15) = happyShift action_8
action_38 (17) = happyShift action_9
action_38 (20) = happyShift action_10
action_38 (25) = happyShift action_22
action_38 (26) = happyShift action_23
action_38 (27) = happyShift action_24
action_38 (29) = happyShift action_25
action_38 (30) = happyShift action_26
action_38 (31) = happyShift action_11
action_38 (32) = happyShift action_12
action_38 (33) = happyShift action_39
action_38 (4) = happyGoto action_19
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (6) = happyShift action_2
action_39 (9) = happyShift action_4
action_39 (10) = happyShift action_5
action_39 (11) = happyShift action_6
action_39 (14) = happyShift action_7
action_39 (15) = happyShift action_8
action_39 (17) = happyShift action_9
action_39 (20) = happyShift action_10
action_39 (31) = happyShift action_11
action_39 (32) = happyShift action_12
action_39 (4) = happyGoto action_50
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (6) = happyShift action_2
action_40 (7) = happyShift action_20
action_40 (8) = happyShift action_21
action_40 (9) = happyShift action_4
action_40 (10) = happyShift action_5
action_40 (11) = happyShift action_6
action_40 (14) = happyShift action_7
action_40 (15) = happyShift action_8
action_40 (17) = happyShift action_9
action_40 (20) = happyShift action_10
action_40 (21) = happyShift action_49
action_40 (25) = happyShift action_22
action_40 (26) = happyShift action_23
action_40 (27) = happyShift action_24
action_40 (29) = happyShift action_25
action_40 (30) = happyShift action_26
action_40 (31) = happyShift action_11
action_40 (32) = happyShift action_12
action_40 (4) = happyGoto action_19
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (16) = happyShift action_48
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (17) = happyShift action_42
action_42 (22) = happyShift action_43
action_42 (23) = happyShift action_44
action_42 (5) = happyGoto action_47
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_19

action_44 _ = happyReduce_20

action_45 (6) = happyShift action_2
action_45 (7) = happyShift action_20
action_45 (8) = happyShift action_21
action_45 (9) = happyShift action_4
action_45 (10) = happyShift action_5
action_45 (11) = happyShift action_6
action_45 (13) = happyShift action_46
action_45 (14) = happyShift action_7
action_45 (15) = happyShift action_8
action_45 (17) = happyShift action_9
action_45 (20) = happyShift action_10
action_45 (25) = happyShift action_22
action_45 (26) = happyShift action_23
action_45 (27) = happyShift action_24
action_45 (29) = happyShift action_25
action_45 (30) = happyShift action_26
action_45 (31) = happyShift action_11
action_45 (32) = happyShift action_12
action_45 (4) = happyGoto action_19
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (6) = happyShift action_2
action_46 (9) = happyShift action_4
action_46 (10) = happyShift action_5
action_46 (11) = happyShift action_6
action_46 (14) = happyShift action_7
action_46 (15) = happyShift action_8
action_46 (17) = happyShift action_9
action_46 (20) = happyShift action_10
action_46 (31) = happyShift action_11
action_46 (32) = happyShift action_12
action_46 (4) = happyGoto action_55
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (16) = happyShift action_54
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (6) = happyShift action_2
action_48 (9) = happyShift action_4
action_48 (10) = happyShift action_5
action_48 (11) = happyShift action_6
action_48 (14) = happyShift action_7
action_48 (15) = happyShift action_8
action_48 (17) = happyShift action_9
action_48 (20) = happyShift action_10
action_48 (31) = happyShift action_11
action_48 (32) = happyShift action_12
action_48 (4) = happyGoto action_53
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (6) = happyShift action_2
action_49 (9) = happyShift action_4
action_49 (10) = happyShift action_5
action_49 (11) = happyShift action_6
action_49 (14) = happyShift action_7
action_49 (15) = happyShift action_8
action_49 (17) = happyShift action_9
action_49 (20) = happyShift action_10
action_49 (31) = happyShift action_11
action_49 (32) = happyShift action_12
action_49 (4) = happyGoto action_52
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (6) = happyShift action_2
action_50 (7) = happyShift action_20
action_50 (8) = happyShift action_21
action_50 (9) = happyShift action_4
action_50 (10) = happyShift action_5
action_50 (11) = happyShift action_6
action_50 (14) = happyShift action_7
action_50 (15) = happyShift action_8
action_50 (17) = happyShift action_9
action_50 (20) = happyShift action_10
action_50 (25) = happyShift action_22
action_50 (26) = happyShift action_23
action_50 (27) = happyShift action_24
action_50 (29) = happyShift action_25
action_50 (30) = happyShift action_26
action_50 (31) = happyShift action_11
action_50 (32) = happyShift action_12
action_50 (33) = happyShift action_51
action_50 (4) = happyGoto action_19
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (6) = happyShift action_2
action_51 (9) = happyShift action_4
action_51 (10) = happyShift action_5
action_51 (11) = happyShift action_6
action_51 (14) = happyShift action_7
action_51 (15) = happyShift action_8
action_51 (17) = happyShift action_9
action_51 (20) = happyShift action_10
action_51 (31) = happyShift action_11
action_51 (32) = happyShift action_12
action_51 (4) = happyGoto action_57
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (6) = happyShift action_2
action_52 (7) = happyShift action_20
action_52 (8) = happyShift action_21
action_52 (9) = happyShift action_4
action_52 (10) = happyShift action_5
action_52 (11) = happyShift action_6
action_52 (14) = happyShift action_7
action_52 (15) = happyShift action_8
action_52 (17) = happyShift action_9
action_52 (20) = happyShift action_10
action_52 (25) = happyShift action_22
action_52 (26) = happyShift action_23
action_52 (27) = happyShift action_24
action_52 (29) = happyShift action_25
action_52 (30) = happyShift action_26
action_52 (31) = happyShift action_11
action_52 (32) = happyShift action_12
action_52 (4) = happyGoto action_19
action_52 _ = happyReduce_11

action_53 (6) = happyShift action_2
action_53 (7) = happyShift action_20
action_53 (8) = happyShift action_21
action_53 (9) = happyShift action_4
action_53 (10) = happyShift action_5
action_53 (11) = happyShift action_6
action_53 (14) = happyShift action_7
action_53 (15) = happyShift action_8
action_53 (17) = happyShift action_9
action_53 (20) = happyShift action_10
action_53 (25) = happyShift action_22
action_53 (26) = happyShift action_23
action_53 (27) = happyShift action_24
action_53 (29) = happyShift action_25
action_53 (30) = happyShift action_26
action_53 (31) = happyShift action_11
action_53 (32) = happyShift action_12
action_53 (4) = happyGoto action_19
action_53 _ = happyReduce_8

action_54 (17) = happyShift action_42
action_54 (22) = happyShift action_43
action_54 (23) = happyShift action_44
action_54 (5) = happyGoto action_56
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (6) = happyShift action_2
action_55 (7) = happyShift action_20
action_55 (8) = happyShift action_21
action_55 (9) = happyShift action_4
action_55 (10) = happyShift action_5
action_55 (11) = happyShift action_6
action_55 (14) = happyShift action_7
action_55 (15) = happyShift action_8
action_55 (17) = happyShift action_9
action_55 (20) = happyShift action_10
action_55 (25) = happyShift action_22
action_55 (26) = happyShift action_23
action_55 (27) = happyShift action_24
action_55 (29) = happyShift action_25
action_55 (30) = happyShift action_26
action_55 (31) = happyShift action_11
action_55 (32) = happyShift action_12
action_55 (4) = happyGoto action_19
action_55 _ = happyReduce_6

action_56 (18) = happyShift action_59
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (6) = happyShift action_2
action_57 (7) = happyShift action_20
action_57 (8) = happyShift action_21
action_57 (9) = happyShift action_4
action_57 (10) = happyShift action_5
action_57 (11) = happyShift action_6
action_57 (14) = happyShift action_7
action_57 (15) = happyShift action_8
action_57 (17) = happyShift action_9
action_57 (18) = happyShift action_58
action_57 (20) = happyShift action_10
action_57 (25) = happyShift action_22
action_57 (26) = happyShift action_23
action_57 (27) = happyShift action_24
action_57 (29) = happyShift action_25
action_57 (30) = happyShift action_26
action_57 (31) = happyShift action_11
action_57 (32) = happyShift action_12
action_57 (4) = happyGoto action_19
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (6) = happyShift action_2
action_58 (9) = happyShift action_4
action_58 (10) = happyShift action_5
action_58 (11) = happyShift action_6
action_58 (14) = happyShift action_7
action_58 (15) = happyShift action_8
action_58 (17) = happyShift action_9
action_58 (20) = happyShift action_10
action_58 (31) = happyShift action_11
action_58 (32) = happyShift action_12
action_58 (4) = happyGoto action_60
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_21

action_60 (6) = happyShift action_2
action_60 (7) = happyShift action_20
action_60 (8) = happyShift action_21
action_60 (9) = happyShift action_4
action_60 (10) = happyShift action_5
action_60 (11) = happyShift action_6
action_60 (14) = happyShift action_7
action_60 (15) = happyShift action_8
action_60 (17) = happyShift action_9
action_60 (20) = happyShift action_10
action_60 (25) = happyShift action_22
action_60 (26) = happyShift action_23
action_60 (27) = happyShift action_24
action_60 (29) = happyShift action_25
action_60 (30) = happyShift action_26
action_60 (31) = happyShift action_11
action_60 (32) = happyShift action_12
action_60 (4) = happyGoto action_19
action_60 _ = happyReduce_18

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (BTrue
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (BFalse
	)

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 4 happyReduction_6
happyReduction_6 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Paren happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 4 happyReduction_11
happyReduction_11 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  4 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Maior happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  4 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (MaiorOuIgual happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  4 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Not happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 9 4 happyReduction_18
happyReduction_18 ((HappyAbsSyn4  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (For happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  5 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn5
		 (TBool
	)

happyReduce_20 = happySpecReduce_1  5 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn5
		 (TNum
	)

happyReduce_21 = happyReduce 5 5 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TFun happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 6;
	TokenAdd -> cont 7;
	TokenAnd -> cont 8;
	TokenTrue -> cont 9;
	TokenFalse -> cont 10;
	TokenIf -> cont 11;
	TokenThen -> cont 12;
	TokenElse -> cont 13;
	TokenVar happy_dollar_dollar -> cont 14;
	TokenLam -> cont 15;
	TokenArrow -> cont 16;
	TokenLParen -> cont 17;
	TokenRParen -> cont 18;
	TokenEq -> cont 19;
	TokenLet -> cont 20;
	TokenIn -> cont 21;
	TokenBoolean -> cont 22;
	TokenNumber -> cont 23;
	TokenColon -> cont 24;
	TokenMul -> cont 25;
	TokenSub -> cont 26;
	TokenOr -> cont 27;
	TokenEq -> cont 28;
	TokenMaior -> cont 29;
	TokenMaiorOuIgual -> cont 30;
	TokenNot -> cont 31;
	TokenFor -> cont 32;
	TokenSemicolon -> cont 33;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 34 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parserError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parserError :: [Token] -> a 
parserError _ = error "Syntax error!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
