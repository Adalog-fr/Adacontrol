with System;
with Ada.Exceptions;
with Ada.Text_IO.Text_Streams;
procedure T_Improper_Initialization is

   procedure Init (I : out Integer) is
   begin
      I := 0;
   end Init;

   type Access_Proc is access procedure (I : out Integer);

   function Get_AP (I : Integer) return Access_Proc is
   begin
      return null;
   end Get_AP;

   ------------------------------
   -- Checking packages bodies --
   ------------------------------
   package My_Pack is
      type Digit is range 0..9;
      BVS : Boolean;
   end My_Pack;

   package body My_Pack is
   --------------------------------
   -- Checking procedures bodies --
   --------------------------------

      -- A_Procedure_Body_Declaration
      -- (IP, IV2) not initialized from the beginning of the body
      procedure Proc_Renaming (IP : out Integer) is                 -- not safely initialized
         type T is
            record
               BC : Boolean;
               IC : Integer;
            end record;
         TV  : T;                                                   -- used before initialization
         X   : Integer;
         IV0 : Integer renames X;
         IV1 : Integer renames IV0;
      begin
         IV1   := 0;
         TV.BC := True;
         if TV.IC = 0 then                                          -- use of uninitialized TV
            IP := 1;
         end if;
      end Proc_Renaming;

      -- A_Procedure_Body_Declaration
      -- IP not initialized from the beginning of the body
      procedure Proc_No_Full_Init (IP : out Integer) is             -- not safely initialized
         IV1, IV2 : Integer;
         IV3      : Integer := 3;
         IV4      : Integer;
         IV5      : Integer := 4;                                   -- unnecessarily initialized
         IV6      : Integer;
         IV7      : Integer;
         IV8, IV9 : Integer;                                        -- used before initialization (x2)
      begin
         IV1 := 1;
         Init (IV4);
         Get_AP(IV8) (IV6);                                         -- use of uninitialized IV8
         Get_AP(IV9).all (IV7);                                     -- use of uninitialized IV9
         if IV1 > IV3 then
            IP := IV3;
         end if;
         IV2 := 2;
         IV5 := 1;
      end Proc_No_Full_Init;

      -- A_Procedure_Body_Declaration
      -- (IP1, IP2, BP) not initialized from the beginning of the body
      procedure Proc_No_Full_Init_2 (DP       : in     Digit;
                                     IP1, IP2 :    out Integer;     -- not safely initialized (x2)
                                     BP       :    out Boolean)     -- not safely initialized
      is
         pragma Unreferenced (DP);
         FV1         : Float := 1.025;
         Pi          : constant Float := 3.141593;
         Gold_Number : constant Float := 1.618034;
         CV1         : Character;
         CV2         : Character := 'x';                            -- unnecessarily initialized
      begin
         CV1 := 'y';
         if 2.0 * Gold_Number + FV1 > Pi + 2.0 * FV1 then
            CV2 := 'y';
         else
            CV2 := 'n';
         end if;
         if CV1 = 'a' then
            IP1 := Integer (Gold_Number);
         else
            IP2 := Integer (Pi);
         end if;
         for I in 1 .. 10 loop
            null;
         end loop;
         BP := (CV1 = CV2);
      end Proc_No_Full_Init_2;

      ---------------------------
      -- Checking tasks bodies --
      ---------------------------
      task type TT is
         entry Entry_1 (IP : out Integer);
         entry Entry_2 (IP : out Integer);
      end TT;

      task body TT is
         Two      : constant Integer := 2;
         Five     : constant Integer := 5;
         IV1      : Integer;                                        -- not safely initialized
         IV2, IV3 : Integer;                                        -- not safely initialized (x2)
         B        : Boolean;                                        -- used before initialization
      begin
         B := B;                                                    -- use of uninitialized
         accept Entry_1 (IP : out Integer) do                       -- used before initialization
            IV1 := IP + Two;                                        -- use of uninitialized
            case B is
               when True =>
                  IV2 := Two;
                  IP  := Five;
               when False =>
                  IV2 := Five;
                  IP  := Two;
            end case;
         end Entry_1;

         accept Entry_2 (IP :    out Integer) do
            IP := Five;
         end Entry_2;

         accept Entry_2 (IP :    out Integer) do                 -- not safely initialized
            null;
         end Entry_2;

         accept Entry_2 (IP :    out Integer);                   -- not safely initialized
      end TT;


      ---------------------------
      -- Checking entry bodies --
      ---------------------------
      protected type PT is
         entry Set_Value (DP : in     Digit);
         entry Get_Value (DP :    out Digit);
         entry Is_Equal  (DP : in     Digit;
                          BP :    out Boolean);
         procedure Reset;
      private
         DV     : Digit;
         Is_Set : Boolean := False;
      end PT;

      protected body PT is
         entry Set_Value (DP : in     Digit) when not Is_Set is
         begin
            Is_Set := True;
            DV     := DP;
         end Set_Value;

         entry Get_Value (DP :    out Digit) when Is_Set is
         begin
            Is_Set := False;
            DP     := DV;
         end Get_Value;

         entry Is_Equal  (DP : in     Digit;
                          BP :    out Boolean)                      -- not safely initialized
         when Is_Set
         is
         begin
            if DP = DV then
               BP := True;
            end if;
         end Is_Equal;

         procedure Reset is
            IV : Integer;                                          -- not safely initialized
            B  : Boolean := False;
         begin
            case B is
               when True =>
                  IV := 0;
               when False =>
                  B := True;
            end case;
         end Reset;
      end PT;

      Pi  : constant Float := 3.141593;
      BVB : Boolean;

   begin
      BVS := False;
      if Pi = 3.141593 then
         BVB := True;
      else
         BVB := False;
      end if;
   end My_Pack;

   -- check case of branches with raise
   procedure Check_Raise (V1, V2 : out Integer; X : Integer) is
      use Ada.Exceptions;
   begin
      if X = 1 then
         V1 := 1;
      elsif X = 2 then
         raise Constraint_Error;
      elsif X = 3 then
         Raise_Exception (Program_Error'Identity);
      else
         V1 := 2;
      end if;

      case X is
         when 1 =>
            V2 := 1;
         when 2 =>
            raise Constraint_Error;
         when 3 =>
            Raise_Exception (Program_Error'Identity);
         when others =>
            V2 := 2;
      end case;
   end Check_Raise;

   -- case of separate body (with and Without Explicit Spec)
   procedure Sep1 (Var1, Var2 : out Integer);
   procedure Sep1 (Var1, Var2 : out Integer) is separate;
   procedure Sep2 (Var1, Var2 : out Integer) is separate;

   -- Case of attributes
   type Str_Ptr is access String;
   procedure Attr (S : out String;                         -- not safely initialized (x1)
                   I : out Integer; AS : out Str_Ptr) is   -- used before initialization (x2)
      S2 : String (S'Range);                               -- not safely initialized (x1)
      CI : Integer;
      A  : System.Address;
      function F return String is
      begin
         return "abc";
      end F;
   begin
      CI := S (I)'Alignment;                           -- use of uninitialized I
      CI := S (S2 (I)'Alignment)'Alignment;            -- use of uninitialized I
      CI := S(1..I)'First;                             -- use of uninitialized I
      CI := S(Integer range 1..I)'First;               -- use of uninitialized I
      A  := Attr'Address;
      CI := F'First;
      A  := AS'Address;
      CI := AS'First;                                  -- use of uninitialized AS
   end Attr;

   -----------------------------------------
   -- Checking extended return statements --
   -----------------------------------------
   function Extended_Return return Integer is separate;

begin

   ---------------------
   -- Checking blocks --
   ---------------------

   declare
      Pi          : constant Float := 3.141593;
      Gold_Number : constant Float := 1.618034;

      IV : Integer;                                                   -- not safely initialized
   begin
      if (Pi ** 2) - Gold_Number > 3.0 then
         IV := Integer ((Pi ** 2) - 3.0 * (Gold_Number ** 2));
      end if;
   end;

   ------------------------
   -- Checking renamings --
   ------------------------
B1:
   declare
      I, J, K, L : Positive;                                -- used before initialization (x4)
      type Rec is
         record
            F : String (1 .. 10);
         end record;
      type Acc_Rec is access all Rec;
      R1 : Rec;
      A1, A2 : Acc_Rec;                                     -- used before initialization (x2)

      function F (Param : Integer) return Rec is
      begin
         return (F => (others => ' '));
      end F;

      function G (Param : Integer) return Acc_Rec is
      begin
         return null;
      end G;

      Ren1 : Character renames T_Improper_Initialization.B1.R1.F (I); -- use of uninitialized I
      Ren2 : String    renames T_Improper_Initialization.B1.A1.F;
      Ren3 : String    renames T_Improper_Initialization.B1.A2.all.F;
      Ren4 : Character renames F (J).F (K);                           -- use of uninitialized J, K
      Ren5 : String    renames G (L).F;                               -- use of uninitialized L
   begin
      R1 := (F => (1..10 => ' '));
   end B1;

   ---------------------------------
   -- checking access and limited --
   ---------------------------------
B2 :
   declare
      type Rec1 is limited null record;
      R1, R2 : Rec1;

      package Pack is
         type Lim is limited private;
      private
         type Lim is new Integer;
      end Pack;
      L1, L2 : Pack.Lim;
      TL1 : array (1 .. 10) of Pack.Lim;
      type Tabl is array (1 .. 10) of Pack.Lim;
      TL2 : Tabl;

      type Acc is access Integer;
      type Tab1 is array (1 .. 10) of Acc;
      type Tab2 is array (1 .. 10) of Tab1;

      T1 : Tab1;
      T2 : Tab2;
   begin
      null;
   end B2;

   ----------------------------------
   -- Checking breaking statements --
   ----------------------------------
   declare
      I1, I2 : Integer;     -- not safely initialized I1 (I2 ok)
      B  : Boolean := True;
   begin
      case B is
         when True =>
            if B then
               I1 := 1;
               I2 := 1;
            elsif B then
               return;
               I1 := 3;
            else
               I2 := 2;
            end if;
         when False =>
            I1 := 0;
            I2 := 0;
      end case;
   end;

   declare
      X : Integer := 1;
      V : Integer;     -- not safely initialized
   begin
      if X = 0 then
         case X is
            when 1 =>
               V := 1;
               if X = 1 then
                  goto Elsewhere;
               end if;
            when others =>
               V := 0;
         end case;
      end if;

   <<Elsewhere>>
      X := 0;
   end;

   ----------------------------------
   -- Checking access from package --
   ----------------------------------
   declare
      A : Integer;               -- used before initialization A
      S : String (1 .. 10);      -- not safely initialized S

      for A'Address use S'Address;

      package Pack is
         B : Integer := A;       -- use of uninitialized A
      end Pack;

      package body Pack is
         V, W : Integer;            -- OK (package variable)
      begin
         V := A;                 -- use of uninitialized A
         S (A) := 'c';           -- use of uninitialized A
      end Pack;
   begin
      A := 1;
   end;

   -----------------------------------
   -- Checking attribute procedures --
   -----------------------------------
   declare
      use Ada.Text_IO, Ada.Text_IO.Text_Streams;
      F : File_Type;
      V1, V2 : Integer;                -- used before initialization V2
   begin
      Integer'Read  (Stream (F), V1);  -- OK

      Integer'Write (Stream (F), V2);  -- use of uninitialized V2
      Integer'Read  (Stream (F), V2);
   end;
end T_Improper_Initialization;
