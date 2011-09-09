separate (T_Array_Declarations)
procedure Index_Types is
   type Enum is (A,B,C);
   type Int is range 1..10;
   subtype Sub_Int  is Int range 2..8;
   subtype Sub_Int2 is Sub_Int;
   type New_Int is new Sub_Int;
   type Uns is mod 256;

   type A1 is array (Enum)            of Integer; -- T1, T5
   type A2 is array (Int range 5..10) of Integer; -- T2, T6, lower bound, array dimension
   type A3 is array (Sub_Int)         of Integer; -- T2, T4, T6, lower bound, array dimension
   type A4 is array (Uns)             of Integer; -- T3, T7, array dimension
   type A5 is array (New_Int)         of Integer; -- T6, lower bound, array dimension
   type A6 is array (Sub_Int2)        of Integer; -- T2, T6, lower bound, array dimension

   V1: array (Int'Range)             of Integer;  -- T2, T6, array dimension
   V2: array (Int'Base'Base'Range)   of Integer;  -- T2, T6
   V3: array (Int'First .. Int'Last) of Integer;  -- T2, T6, array dimension
   V4 : array (1 .. 10)              of Integer;  -- T6, array dimension
   V5 : array (A..B)                 of Integer;  -- T1, T5, lower bound

   type AA1 is array (Enum, Sub_Int)           of Integer; -- TT1, more than 1 dimension, lower bound, array dimension
   type AA2 is array (A..B, 1..10)             of Integer; -- TT1, more than 1 dimension, lower bound, array dimension
   type AA3 is array (V5'range, AA1'Range (2)) of Integer; -- TT1, more than 1 dimension, lower bound x2, array dimension

   type AAA1 is array (Enum,    Sub_Int, Uns) of Integer; -- TTT1,             TTT4, more than 1 dimension, lower bound, array dimension x2
   type AAA2 is array (Int,     Sub_Int, Uns) of Integer; -- TTT1, TTT2, TTT3, TTT4, more than 1 dimension, array dimension x3, lower bound
   type AAA3 is array (New_Int, Sub_Int, Uns) of Integer; -- TTT1, TTT2,       TTT4, more than 1 dimension, lower bound x2, array dimension x3
   type AAA4 is array (Int,     Sub_Int, Int) of Integer; --       TTT2, TTT3, TTT4, more than 1 dimension, array dimension x3, lower bound

begin
   null;
end Index_Types;
