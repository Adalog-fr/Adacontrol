separate (T_Array_Declarations)
procedure Component is
   type T1 is array (1 .. 4) of Character;  -- Standard.Character, unpacked unsized (), Index_T6
   V1: array (1..4) of Character;           -- Standard.Character, unpacked unsized ()
   subtype UC is Character range 'A'..'Z';
   type T2 is array (1 .. 4) of UC;         -- Standard.Character, unpacked unsized (), Index_T6

   type T3 is array (1 .. 4) of T2;         -- Array, Index_T6

   type Der is new T3;
   type T4 is array (1..4) of Der;          -- Array, Index_T6

   task type TT;
   task body TT is
   begin
      null;
   end TT;
   V2 : array (1 .. 4) of TT;               -- Task, Index_T6

   protected type Prot is
      procedure Proc;
   end Prot;
   protected body Prot is
      procedure Proc is
      begin
         null;
      end;
   end Prot;
   type T5 is array (1..4) of Prot;          -- Protected, Index_T6

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

   type T6 is array (1 .. 4) of Priv;        -- Private, Index_T6
   type T7 is array (1 .. 4) of Node;        -- Record, Index_T6
   type T8 is array (1 .. 4) of Link;        -- Access, Index_T6

   type Tagged_T1 is tagged null record;
   type Tagged_T2 is new Tagged_T1 with null record;
   type T9 is array (1 .. 4) of Tagged_T1;   -- Tagged, Index_T6
   type T10 is array (1 .. 4) of Tagged_T2;  -- Tagged, Index_T6

   type T11 is array (1 .. 4) of Duration;   -- Delta, Index_T6

   type Enum is (A, B, C);
   type T12 is array (1 .. 4) of Enum;       -- unpacked unsized (), Index_T6
   type T13 is array (1 .. 4) of Enum;       -- packed unsized (), Index_T6
   pragma Pack (T13);
   type T14 is array (1 .. 4) of Enum;       -- unpacked sized (), Index_T6
   for T14'Size use 4*8;
   type T15 is array (1 .. 4) of Enum;       -- packed, sized (), Index_T6
   pragma Pack (T15);
   for T15'Size use 4*8;
   type T16 is array (1 .. 4) of Enum;       -- component_sized (), unpacked unsized (), Index_T6
   for T16'Component_Size use 8;

   generic
      type Real is digits <>;
   package Gen1 is
      type T17 is array (1 .. 4) of Real;    -- digits, Index_T6
   end Gen1;
begin
   null;
end Component;
