separate (T_Record_Declarations)
procedure Component is
   subtype UC is Character range 'A' .. 'Z';

   type Enum is (A, B, C);
   type Uns  is mod 256;

   type Enum_TabU is array (1 .. 4) of Enum;

   type Enum_TabP is new Enum_TabU;
   pragma Pack (Enum_TabP);

   type Der is new String (1 .. 4);

   task type TT;
   task body TT is
   begin
      null;
   end TT;

   protected type Prot is
      procedure Proc;
   end Prot;
   protected body Prot is
      procedure Proc is
      begin
         null;
      end;
   end Prot;

   type Tagged_T1 is tagged null record;
   type Tagged_T2 is new Tagged_T1 with null record;

   type Rec1 is
      record
         F1, F2 : Character;              -- Standard.Character, () x2
         F3     : UC;                     -- Standard.Character, ()
         F4     : String (1..10);         -- packed unsized array
         F5     : Der := (others => ' '); -- packed unsized array, initialized array
         F6     : TT;                     -- Task
         F7     : Prot;                   -- Protected
      end record;

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

   type Rec2 is
      record
         F1 : Priv;        -- Private
         F2 : Node;        -- Record
         F3 : Link;        -- Access
         F4 : Tagged_T1;   -- Tagged
         F5 : Tagged_T2;   -- Tagged
         F6 : Duration;    -- Delta
      end record;

   type Rec3 is
      record
         F1 : Enum_TabU;       -- unpacked unsized array
         F2 : Enum_TabP;       --   packed unsized array
         F3 : Enum_TabU;       -- unpacked   sized array
         F4 : Enum_TabP;       --   packed,  sized array
         F5 : Node;            -- unaligned record, record
      end record;
   for Rec3 use
      record
         F3 at 8 range  1 .. 32;
         F4 at 8 range 40 .. 48;
         F5 at 0 range  1 .. 64;
      end record;

   generic
      type Real     is digits <>;
      type Discrete is (<>);
   package Gen1 is
      type Rec4 is
         record
            F1 : Real;     -- uninitialized digits
            F2 : Enum;     -- ()
            F3 : Discrete; -- ()
         end record;
   end Gen1;

   type Rec4 (D : Boolean) is
      record
         F1 : Integer;                -- not in variant range
         F2 : Uns;
         case D is
            when False =>
               V1 : Integer;
               V2 : Uns;              -- in variant mod
            when True =>
               V3 : Integer;
               V4 : Uns;              -- in variant mod
               case D is
                  when False =>
                     null;
                  when True =>
                     VV1 : Integer;
                     VV2 : Uns;        -- in variant mod
               end case;
         end case;
      end record;

begin
   null;
end Component;
