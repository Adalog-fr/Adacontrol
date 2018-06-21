with Ada.Unchecked_Conversion;
package body T_naming_convention is
   procedure T_Cat is separate;

   My_Const  : constant Integer := 1;
   My_Number : constant := 1.0;
   C_Max     : constant := 1; -- OK
   C_1       : constant := 1; -- OK, too short for "all" but rule is root
   My_Max    : constant := 2; --## RULE LINE OFF const1 ## (OK, disabled)

   Xxx   : Integer;                       -- Too_short
   Xxxx  : Integer;                       -- OK
   C_Xxx : constant Integer := Xxx;
   C_Xxx_Static : constant Integer := 1+2;

   -- Casing
   X_Yz : Integer; --OK
   Xz_Y : Integer; --OK
   z : Integer;
   W_z : Integer;
   WW : Integer;
   W_WW : Integer;

   Is_OK        : Boolean;   -- OK
   Has_Correct  : Boolean;   -- OK
   IsIncorrect  : Boolean;   -- Bool_var
   Is_Incorrect : Integer;   -- Not_Bool
   Has_Bad      : Integer;   -- Not_Bool

   type T_Enum is (Enum_A, Enum_B, Enum_C); -- OK
   type T_Enum_Bad is (A, B, C);
   type T_Tagged_Bad is tagged null record;
   type T_Tagged is tagged null record; --OK
   subtype T_Class_Bad is T_Tagged'Class;
   subtype T_Class is T_Tagged'Class; --OK

   type Int_Type is range 1..10;
   type T_Int is range 1..10; -- OK
   type Int_T is range 1..10; -- OK
   Integer : Float;

   type Mod_Bad is mod 255;
   type T_Good_Mod is mod 255;

   type T_Rec           -- OK
     (D_Bool : Boolean; -- OK
      Bad_2  : Boolean)
   is
     record
        Field    : Boolean;
        Rf_Field : Boolean; -- OK
        case D_Bool is
           when True =>
              True_F1  : Boolean;
              Rf_True1 : Boolean; --OK
           when False =>
              case Bad_2 is
                 when True =>
                    True_F2  : Boolean;
                    Rf_True2 : Boolean; --OK
                 when False =>
                    null;
              end case;
        end case;
     end record;

   type T_Rec_2_Tagged is new T_Tagged with --OK
      record
         Field    : Boolean;
         Rf_Field : Boolean; -- OK
      end record;

   type T_Good_Interface is interface; --OK
   type T_Bad_Interf is interface;

   -- Procedure with a spec:
   procedure Bad1_Gen (BadParam: Float);
   procedure Bad1_Gen (BadParam: Float) is
   begin
      null;
   end;

   -- Procedure body without a spec:
   procedure Bad2_Gen  (BadParam: Float) is
   begin
      null;
   end;

   -- Functions
   function Wide_Body return Character is          -- Non_Wide_F
   begin
      return '1';
   end Wide_Body;

   function Wide_Wide_Body return Character is     -- Non_Wide_F
   begin
      return '1';
   end Wide_Wide_Body;

   function Wide_Wide_Image (C_X : Float) return Wide_String is -- Wide_F
   begin
      return Float'Wide_Image (C_X);
   end Wide_Wide_Image;

   function Wide_Image (C_X : Float) return Wide_String is          -- OK
   begin
      return Float'Wide_Image (C_X);
   end Wide_Image;

   function Wide_Wide_Image (C_X : Float) return Wide_Wide_String is -- OK
   begin
      return Float'Wide_Wide_Image (C_X);
   end Wide_Wide_Image;

   subtype T_String10      is String (1..10);
   subtype T_Wide_String10 is Wide_String (1..10);
   function Wide_Conv is new Ada.Unchecked_Conversion (T_String10,      T_String10);      -- Non_Wide_F
   function Wide_Conv is new Ada.Unchecked_Conversion (T_Wide_String10, T_Wide_String10); -- OK

   -- Package
   package Pack is
      type Priv_T is private; -- OK
      type Bad_Priv_T is private;

      type T_Access_Priv_Regular is access Priv_T; -- OK
      type T_Access_Priv_Reg is access Priv_T;
   private
      type Priv_T is new Mod_Bad; -- OK
      type Bad_Priv_T is new Mod_Bad;

      -- Incomplete types
      type T_Incomplete;
      type T_Incomplete_Mod; --OK
      type T_access_Incomplete_Reg is access T_Incomplete;
      type T_access_Incomplete_Regular is access T_Incomplete; --OK
   end Pack;

   package body Pack is
      type T_Incomplete is mod 10;
      type T_Incomplete_Mod is mod 10; -- OK
   end Pack;

   -- Generics
   generic
   procedure Gen_Proc;
   procedure Gen_Proc is
   begin
      null;
   end;

   generic
   procedure Proc_Gen; -- OK
   procedure Proc_Gen is
   begin
      null;
   end;

   procedure New_Gen is new Gen_Proc;
   procedure New_Proc is new Proc_Gen; -- OK

   generic procedure Ren_Gen_Proc renames Gen_Proc;
   generic procedure Proc_Ren_Gen renames Proc_Gen; --OK
   procedure Ren_Bad1_Gen (OtherName: Float) renames Bad1_Gen;
   procedure Ren_Bad2_Gen (OtherName: Float);
   procedure Ren_Bad2_Gen (OtherName: Float) renames Bad2_Gen; -- Rename as body
   procedure Ren_New_Proc renames New_Proc; --OK

   generic
   function SP_Func_GEN return Int_Type;  --OK
   function SP_Func_GEN return Int_Type is
   begin
      return 1;
   end SP_Func_GEN;

   generic
   function proc_Func_GEN return Int_Type;
   function proc_Func_GEN return Int_Type is
   begin
      return 1;
   end proc_Func_GEN;

   -- Protected and tasks
   protected Prot is
      procedure P;
      procedure P_Gen;
      procedure Protec_P; -- OK
   private
      Field : Boolean;
      Pf_Field : Boolean; -- OK
   end Prot;
   protected body Prot is
      procedure P is begin null; end;
      procedure P_Gen is begin null; end;
      procedure Protec_P is begin null; end;
   end Prot;

   task type My_Task is
      entry E;
      entry Task_E; -- OK
   end My_Task;
   task body My_Task is begin null; end;
   type T_New_Task is new My_Task;        -- OK
   subtype T_Sub_New_Task is T_New_Task;  -- OK

   -- Access
   type Acc_1 is access Int_Type;
   type T_Access_Regular is access Float;           --OK
   type Acc_2 is access T_Tagged;
   type T_Access_Tagged is access T_Tagged;         --OK
   type Acc_3 is access T_Tagged'Class;
   type T_1_Access_Class is access T_Tagged'Class;  --OK
   type Acc_4 is access T_Class;
   type T_2_Access_Class is access T_Class;         --OK
   type Acc_5 is access procedure;
   type T_Access_SP is access procedure;            -- OK
   type Acc_6 is access My_Task;
   type T_Access_Task is access My_Task;            -- OK
   type Acc_7 is access T_Sub_New_Task;
   type T_Access_Sub_Task is access T_Sub_New_Task; -- OK

   -- Renaming:
   Enum_Ren_A : T_Enum renames Enum_A; -- OK
   Bad_Enum_Ren_A : T_Enum renames Enum_A;
   type T_Access_String_Regular is access all String;
   type T_Access_String_Const_Regular is access constant String;
   Var_Access1 : T_Access_String_Regular := new String'("Hello");
   Var : aliased String := "Hello";
   Var_Access2 : T_Access_String_Regular := Var'Access;
   Const : aliased constant String := "Hello";
   Const_Access : T_Access_String_Const_Regular := Const'Access;

   Ren1 : String renames Var_Access1.all;          -- OK
   R1 : String renames Var_Access1.all;
   Ren2 : String renames Var_Access2.all;          -- OK
   R2 : String renames Var_Access2.all;
   C_R3 : String renames Const_Access.all;         -- OK
   Ren3 : String renames Const_Access.all;
   C_R4_Static : String renames Const;             -- OK
   C_R5_Static : Character renames C_R4_Static(1); -- OK
   Ren5_Static : Character renames C_R4_Static(1);
   C_R6 : Character renames Ren3(1);               -- OK
   Ren6 : Character renames Ren3(1);

   type Obj_T is
      record
         Rf_1 : String (1 .. 10);
      end record;
   type T_1 is new Obj_T;
   type T_2 is access Obj_T;
   type T_3 is new T_2;
   Obj_A  : T_1;
   Obj_B  : T_2;
   Obj_C  : T_3;

   C1    : Character renames Obj_A.Rf_1 (1);
   Rf_C1 : Character renames Obj_A.Rf_1 (1); -- OK
   C2    : Character renames Obj_B.Rf_1 (1);
   Rf_C2 : Character renames Obj_B.Rf_1 (1);
   C3    : Character renames Obj_C.Rf_1 (1);
   Rf_C3 : Character renames Obj_C.Rf_1 (1);

   CE      : exception renames Constraint_Error;
   CE_ExcR : exception renames Constraint_Error; -- OK

      -- Subtypes and derived types
   subtype ST1 is My_Task;
   subtype T_Task is My_Task;    -- OK
   subtype ST2 is Mod_Bad;
   subtype T_Sub_Mod is Mod_Bad; -- OK

   type DT1 is new My_Task;
   type T_2_Task is new My_Task;               -- OK
   type Dt2 is new Mod_Bad;
   type T_Derived_Mod is new Mod_Bad;          -- OK
   subtype Dt3 is T_Derived_Mod;
   subtype T_Derived_Sub_Mod is T_Derived_Mod; -- OK;

   -- Rules through generic formals
   generic
      type F1 is new T_Enum;
      type F2 is new T_Tagged with private;
      type F3 is private;
      type F4 is tagged private;
   package Gen_Pack is
      type A1 is access F1;
      type T_Access_1_Regular is access F1; -- OK
      type A2 is access F2;
      type T_Access_2_Tagged is access F2;  -- OK
      type A3 is access F3;
      type T_Access_3_Regular is access F3; -- OK
      type A4 is access F4;
      type T_Access_4_Tagged is access F4; -- OK
   end Gen_Pack;

   -- Separate unit
   procedure Sep is separate;

   -- Local/Global
   procedure G_Proc is
   begin
      null;
   end G_Proc;

   package body Inner is
      procedure G_Proc is
         L_X3 : Standard.Integer;
         G_X3 : Standard.Integer;
      begin
         null;
      end G_Proc;
   end Inner;

   -- Mantis 0000008
   type T1 is tagged null record;

   package Pack08 is
      type T2 is new T1 with private;
   private
      type T2 is new T1 with null record;
   end;

   type D1 is abstract new Pack08.T2 with
      record
	 F : Duration;
      end record;

   type D2 is new D1 with
      record
	 null;
      end record;

   X : D2;
   Y : Duration renames X.F;
   -- End Mantis 0000008


begin
<<B1>>
Block :
   begin
      goto label;
   end block;
   <<Label>> --OK
   <<La_Bel>>
  L : for I in 1..10 loop
     null;
     end loop L;
  G_Label : for C_I in 1..10 loop -- OK
     null;
     end loop G_Label;
end T_naming_convention;
