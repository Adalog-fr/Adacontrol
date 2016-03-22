separate (T_Array_Declarations)
procedure Component is
   type T1  is array (1 .. 4) of Character;      -- Index_T6, Standard.Character, () with not pack not size
   type T1a is array (1 .. 4) of Character'Base; -- Index_T6, Standard.Character'Base,  () with not pack not size
   V1 : array (1 .. 4) of Character;             -- Index_T6, Standard.Character, () with not pack not size
   subtype UC is Character range 'A'..'Z';
   type T2 is array (1 .. 4) of UC;              -- Index_T6, Standard.Character, () with not pack not size

   type T3 is array (1 .. 4) of T2;              -- Index_T6, Array

   type Der is new T3;
   type T4 is array (1 .. 4) of Der;             -- Index_T6, Array

   task type TT;
   task body TT is
   begin
      null;
   end TT;
   V2 : array (1 .. 4) of TT;                    -- Index_T6, Task

   protected type Prot is
      procedure Proc;
   end Prot;
   protected body Prot is
      procedure Proc is
      begin
         null;
      end;
   end Prot;
   type T5 is array (1..4) of Prot;          -- Index_T6, Protected

   package Pack is
      type Priv is private;
      type Node;
      type Link is access Node;
      type Node is
         record
            Next : Link;
         end record;
   private
      type Priv is new Integer;
   end Pack;
   use Pack;

   type T6 is array (1 .. 4) of Priv;        -- Index_T6, Private
   type T7 is array (1 .. 4) of Node;        -- Index_T6, Record
   type T8 is array (1 .. 4) of Link;        -- Index_T6, Access

   type Tagged_T1 is tagged null record;
   type Tagged_T2 is new Tagged_T1 with null record;
   type T9 is array (1 .. 4) of Tagged_T1;   -- Index_T6, Tagged
   type T10 is array (1 .. 4) of Tagged_T2;  -- Index_T6, Tagged

   type T11 is array (1 .. 4) of Duration;   -- Index_T6, Delta

   type Enum is (A, B, C);
   type T12 is array (1 .. 4) of Enum;       -- Index_T6, () with not pack not size
   type T13 is array (1 .. 4) of Enum;       -- Index_T6, () with pack not size
   pragma Pack (T13);
   type T14 is array (1 .. 4) of Enum;       -- Index_T6, () with not pack size
   for T14'Size use 4*8;
   type T15 is array (1 .. 4) of Enum;       -- Index_T6, () with pack size
   pragma Pack (T15);
   for T15'Size use 4*8;
   type T16 is array (1 .. 4) of Enum;       -- Index_T6, () with component_size, () with not pack not size
   for T16'Component_Size use 8;

   generic
      type Real is digits <>;
   package Gen1 is
      type T17 is array (1 .. 4) of Real;    -- Index_T6, digits
   end Gen1;
begin
   null;
end Component;
