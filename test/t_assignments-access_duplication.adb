with Ada.Finalization;
separate (t_assignments)
procedure Access_Duplication is
   --## Rule off repeated ## We make many crazy consecutive assignments in this test...
   type Acc is access Integer;
   subtype Sub_Acc is Acc;
   type Acc_Enum is access Boolean;
   type Acc_Float is access Float;
   type Acc_Proc is access procedure;
   type Acc_Func is access function return Integer;

   V1 : Acc;
   V2 : Sub_Acc := V1;                   -- Uncontrolled duplication
   V3 : access Acc;

   type Arr1 is array (1 .. 10) of Sub_Acc;
   A1 : Arr1;
   A2 : Arr1 := (Arr1'(A1));             -- Uncontrolled duplication

   type Rec1 is  -- use deep access type
      record
         I : Integer;
         A : Arr1 := A1;
      end record;
   type Rec2 is  -- use anonymous access type
      record
         I : Integer;
         A : access Integer;
      end record;
   type Rec3 is                          -- For box components
      record
         F : Rec1;
      end record;
   type Arr2 is array (1..3) of Rec1;    -- For box components

   R11 : Rec1;
   R12 : Rec1 := R11;                    -- Uncontrolled duplication
   A21 : Arr2;
   R31 : Rec3;

   type Acc_Rec is access Rec1;
   AR : Acc_Rec;

   function F1 return Integer is
   begin
      return 1;
   end;

   function F2 return Arr1 is
   begin
      return A1;
   end;

   function F3 return Rec1 is
   begin
      return R11;
   end;

   generic
      type Form_Acc is access Integer;
      type Target is private;
   package Gen is
      type Gen_Acc is access Target;
      GV1 : Form_Acc;
      type New_FA is new Form_Acc;
      GV2 : Form_Acc;
      GV3 : Form_Acc;
   end Gen;
   package Inst is new Gen (Acc, Integer);
   use Inst;
   IV1  : New_FA;
   IV21 : Gen_Acc;
   IV22 : Gen_Acc;

   Cond : Boolean;

   type Contrld_Acc is new Ada.Finalization.Controlled with
      record
         Ptr : Acc;
      end record;
   Ctrl1 : Contrld_Acc := (Ada.Finalization.Controlled with Ptr => V1);  -- Controlled duplication
   Ctrl2 : Contrld_Acc := Ctrl1;                                         -- Controlled duplication

   type Incl_Ctld is
      record
         F : Contrld_Acc;
      end record;
   IC1 : Incl_Ctld;
   IC2 : Incl_Ctld := IC1;                                               -- Controlled duplication

   -- Tagged types
   package T_Tagged is
      type Tag1 is tagged
         record
            F : Acc;
         end record;

      type Tag2 is tagged private;
      type Tag3 is tagged private;
      type Tag4 is tagged private;
   private
      type Tag0 is tagged null record;
      type Tag2 is new Tag1 with null record;
      type Tag3 is tagged
         record
            F : Acc;
         end record;
      type Tag4 is new Tag0 with
         record
            F : Acc;
         end record;
   end T_Tagged;

   Tag11, Tag12 : T_Tagged.Tag1;
   Tag21, Tag22 : T_Tagged.Tag1;
   Tag31, Tag32 : T_Tagged.Tag1;
   Tag41, Tag42 : T_Tagged.Tag1;

      -- Exceptions
   Acc_Enum1 : Acc_Enum;
   Acc_Enum2 : Acc_Enum := Acc_Enum1;                                    -- OK (has not)
   Acc_Float1 : Acc_Float;
   Acc_Float2 : Acc_Float := Acc_Float1;                                 -- OK (has not)

   procedure Proc is null;
   Acc_Sp1 : Acc_Proc := Proc'Access;
   Acc_Sp2 : Acc_Proc := Acc_Sp1;                                        -- Uncontrolled duplication
   Acc_Sp3 : Acc_Func := F1'Access;
   Acc_Sp4 : Acc_Func := Acc_Sp3;                                        -- OK (has not)
begin
   -- Simple case
   V1 := null;
   V1 := new Integer;
   V1 := new Integer'(2);
   V2 := V1;                             -- Uncontrolled duplication
   V2 := Acc'((((V1))));                 -- Uncontrolled duplication

   V1     := V3.all;                     -- Uncontrolled duplication
   V3.all := V1;                         -- Uncontrolled duplication

   -- Composite
   A1 := (others => null);
   A1 := (1..10  => new Integer);
   A1 := (others => new Integer'(2));
   A2 := A1;                             -- Uncontrolled duplication
   A2 := (others => V1);                 -- Uncontrolled duplication
   A1 (1)    := A1 (2);                  -- Uncontrolled duplication
   A1 (1..3) := A2 (1..3);               -- Uncontrolled duplication


   R11 := (1, (others => null));
   R11 := (1, (others => new Integer'(5)));
   R11 := (1, (others => <>));
   R11 := (1, A1);                       -- Uncontrolled duplication
   R11 := (1, (others => V1));           -- Uncontrolled duplication
   R12 := R11;                           -- Uncontrolled duplication
   R12 := AR.all;                        -- Uncontrolled duplication
   R12.I := AR.all.I;
   AR   := new Rec1'(R11);               -- Uncontrolled duplication
   AR   := new Rec1'(1, (others => null));

   -- Function calls
   R11.I := F1;
   R11.A := F2;                          -- Possible uncontrolled duplication
   R11   := F3;                          -- Possible uncontrolled duplication

   -- Conditional expressions
   R11 := (if Cond
          then (1, (others => null))
          else (1, (others => new Integer'(5))));
   R11 := (if Cond
          then (1, (others => null))
          else (1, A1));                 -- Uncontrolled duplication
   R11 := (case Cond is
             when False => (1, (others => null)),
             when True  => (1, (others => new Integer'(5))));
   R11 := (case Cond is
          when False => (1, (others => null)),
          when True  => (1, A1));        -- Uncontrolled duplication

   -- Box expressions
   R31 := (others => <>);                -- Possible uncontrolled duplication
   A21 := (others => <>);                -- Possible uncontrolled duplication

   -- Generics
   V1   := GV2;                           -- Uncontrolled duplication
   V1   := Acc (IV1);                     -- Uncontrolled duplication
   GV3  := GV2;                           -- Uncontrolled duplication
   IV22 := IV21;                          -- Uncontrolled duplication

   -- Controlled types
   Ctrl1 := Ctrl2;                       -- Controlled duplication
   IC1   := IC2;                         -- Controlled duplication

   -- Tagged types
   Tag11 := Tag12;                       -- Uncontrolled_Duplication
   Tag21 := Tag22;                       -- Uncontrolled_Duplication
   Tag31 := Tag32;                       -- Uncontrolled_Duplication
   Tag41 := Tag42;                       -- Uncontrolled_Duplication

   -- Exceptions
   Acc_Enum2  := Acc_Enum1;              -- OK (has not)
   Acc_Float2 := Acc_Float1;             -- OK (has not)
   Acc_Sp1    := Acc_Sp2;                -- Uncontrolled duplication
   Acc_Sp3    := Acc_Sp4;                -- OK (has not)
end access_duplication;
