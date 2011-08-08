separate (T_Array_Declarations)
procedure Component is
   type T1 is array (1 .. 4) of Character;  -- Standard.Character, unpacked unsized ()
   V1: array (1..4) of Character;           -- Standard.Character, unpacked unsized ()
   subtype UC is Character range 'A'..'Z';
   type T2 is array (1 .. 4) of UC;         -- Standard.Character, unpacked unsized ()

   type T3 is array (1 .. 4) of T2;         -- Array

   type Der is new T3;
   type T4 is array (1..4) of Der;          -- Array

   task type TT;
   task body TT is
   begin
      null;
   end TT;
   V2 : array (1 .. 4) of TT;               -- Task

   protected type Prot is
      procedure Proc;
   end Prot;
   protected body Prot is
      procedure Proc is
      begin
         null;
      end;
   end Prot;
   type T5 is array (1..4) of Prot;          -- Protected

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

   type T6 is array (1 .. 4) of Priv;        -- Private
   type T7 is array (1 .. 4) of Node;        -- Record
   type T8 is array (1 .. 4) of Link;        -- Access

   type Tagged_T1 is tagged null record;
   type Tagged_T2 is new Tagged_T1 with null record;
   type T9 is array (1 .. 4) of Tagged_T1;   -- Tagged
   type T10 is array (1 .. 4) of Tagged_T2;  -- Tagged

   type T11 is array (1 .. 4) of Duration;   -- Delta

   type Enum is (A, B, C);
   type T12 is array (1 .. 4) of Enum;       -- unpacked unsized ()
   type T13 is array (1 .. 4) of Enum;       -- packed unsized ()
   pragma Pack (T13);
   type T14 is array (1 .. 4) of Enum;       -- unpacked sized ()
   for T14'Size use 4*8;
   type T15 is array (1 .. 4) of Enum;       -- packed, sized ()
   pragma Pack (T15);
   for T15'Size use 4*8;
   type T16 is array (1 .. 4) of Enum;       -- component_sized (), unpacked unsized ()
   for T16'Component_Size use 8;

   generic
      type Real is digits <>;
   package Gen1 is
      type T17 is array (1 .. 4) of Real;
   end Gen1;
begin
   null;
end Component;
