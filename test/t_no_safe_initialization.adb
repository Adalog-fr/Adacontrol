procedure T_No_Safe_Initialization is

   procedure Init (I : out Integer) is
   begin
      I := 0;
   end Init;

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
      procedure Proc_Renaming (IP : out Integer) is                             -- should trigger
         type T is
            record
               BC : Boolean;
               IC : Integer;
            end record;
         TV  : T;                                                               -- should trigger
         IV0 : Integer renames TV.IC;
         IV1 : Integer renames IV0;
      begin
         IV1   := 0;
         TV.BC := True;
         if TV.IC = 0 then
            IP := 1;
         end if;
      end Proc_Renaming;

      -- A_Procedure_Body_Declaration
      -- (IP, IV2) not initialized from the beginning of the body
      procedure Proc_No_Full_Init (IP : out Integer) is                         -- should trigger
         IV1, IV2 : Integer;                                                    -- should trigger
         IV3      : Integer := 3;
         IV4      : Integer;
      begin
         IV1 := 1;
         Init (IV4);
         if IV1 > IV3 then
            IP := IV3;
         end if;
         IV2 := 2;
      end Proc_No_Full_Init;

      -- A_Procedure_Body_Declaration
      -- (IP1, IP2, BP, CV2) not initialized from the beginning of the body
      procedure Proc_No_Full_Init_2 (DP       : in     Digit;
                                     IP1, IP2 :    out Integer;                 -- should trigger (x2)
                                     BP       :    out Boolean)                 -- should trigger
      is
         pragma Unreferenced (DP);
         FV1         : Float := 1.025;
         Pi          : constant Float := 3.141593;
         Gold_Number : constant Float := 1.618034;
         CV1, CV2    : Character;                                               -- should trigger (x1)
      begin
         CV1 := 'y';
         if 2.0 * Gold_Number + FV1 > Pi + 2.0 * FV1 then
            CV2 := 'y';
         else
            CV2 := 'n';
         end if;
         BP := (CV1 = CV2);
         if BP then
            IP1 := Integer (Gold_Number);
         else
            IP2 := Integer (Pi);
         end if;
      end Proc_No_Full_Init_2;


      ------------------------------
      -- Checking function bodies --
      ------------------------------

      -- A_Procedure_Body_Declaration
      -- (IV2, IV3) not initialized from the beginning of the body
      function Func_Not_Full_Init (IP : in Integer) return Integer is
         Two      : constant Integer := 2;
         Five     : constant Integer := 5;
         IV1      : Integer := IP + Two;
         IV2, IV3 : Integer;                                                    -- should trigger (x2)
      begin
         if IV1 mod Five > IP then
            IV2 := Two;
            IV3 := Five;
         else
            IV2 := Five;
            IV3 := Two;
         end if;
         return IV1 + IV2 / IV3;
      end Func_Not_Full_Init;


      ---------------------------
      -- Checking tasks bodies --
      ---------------------------
      task type TT is
         entry Entry_1 (IP : in     Integer);
         entry Entry_2 (IP :    out Integer);
      end TT;

      task body TT is
         Two      : constant Integer := 2;
         Five     : constant Integer := 5;
         IV1      : Integer;                                                    -- should trigger
         IV2, IV3 : Integer;                                                    -- should trigger (x2)
      begin
         accept Entry_1 (IP : in     Integer) do
            IV1 := IP + Two;
            if IV1 mod Five > IP then
               IV2 := Two;
               IV3 := Five;
            else
               IV2 := Five;
               IV3 := Two;
            end if;
         end Entry_1;

         accept Entry_2 (IP :    out Integer) do
            IP := Five;
         end Entry_2;

         accept Entry_2 (IP :    out Integer) do                             -- should trigger
            null;
         end Entry_2;

         accept Entry_2 (IP :    out Integer);                               -- should trigger
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
                          BP :    out Boolean)                                  -- should trigger
         when Is_Set
         is
         begin
            if DP = DV then
               BP := True;
            else
               BP := False;
            end if;
         end Is_Equal;

         procedure Reset is
         begin
            Is_Set := False;
         end Reset;
      end PT;

      Pi  : constant Float := 3.141593;
      BVB : Boolean;                                                            -- should trigger

   begin
      BVS := False;
      if Pi = 3.141593 then
         BVB := True;
      else
         BVB := False;
      end if;
   end My_Pack;

   -- case of separate body
   package Sep is
      Var1, Var2 : Integer;                                                 -- should trigger (x1)
   end Sep;
   package body Sep is separate;

begin

   ---------------------
   -- Checking blocks --
   ---------------------

   declare
      Pi          : constant Float := 3.141593;
      Gold_Number : constant Float := 1.618034;

      IV : Integer;                                                             -- should trigger
   begin
      if (Pi ** 2) - Gold_Number > 3.0 then
         IV := Integer ((Pi ** 2) - 3.0 * (Gold_Number ** 2));
      end if;
   end;

end T_No_Safe_Initialization;
