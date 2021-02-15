with Ada.Text_IO;
with X_Renaming_Declarations;
procedure T_Renaming_Declarations is

   --
   -- Local types
   --

   type Rec is
      record
         Comp : Integer;
         Str  : String (1 .. 10);
      end record;
   type Access_Str is access String;
   type Access_Rec is access Rec;

   --
   -- Declarations to be renamed
   --

   V : Integer;
   R : Rec;
   S : String (1 .. 120);
   Ptr1 : Access_Str;
   Ptr2 : Access_Rec;

   function F (I : Integer) return Integer is (0);

   procedure Proc1 is
   begin
      null;
   end Proc1;
   procedure Proc2;                                                    -- body provided by renaming

   package Pack is
   end Pack;

   E : exception;

   generic
   procedure Gen;
   procedure Gen is
   begin
      null;
   end Gen;

   generic
   function Gen_F return Integer;
   function Gen_F return Integer is (0);

   --
   -- Start of actual test
   --

   Ren_V     : Integer   renames V;                                    -- Obj, Synonym, Global_All
   Ren2_V    : Integer   renames T_Renaming_declarations.V;            -- Obj, Synonym, Global_All
   Ren3_V    : Integer   renames Ren_V;                                -- Obj, Synonym, Ren_Ren, Global_All
   Ren_Call  : Integer   renames F (1);                                -- Call, Global_All
   S_Bis     : String    renames S;                                    -- Obj, Synonym, Global_All
   S_Slice   : String    renames S (1 .. 3);                           -- Obj, Synonym, Global_All
   C         : Character renames S (1);                                -- Obj, Synonym, Global_All
   C_Bis     : Character renames S_Bis (1);                            -- Obj, Synonym, Ren_Ren, Global_All
   Rc        : Integer   renames R.Comp;                               -- Obj, Synonym, Global_All
   Comp      : Integer   renames R.Comp;                               -- Obj, Synonym, Global_All
   Ccomp     : Character renames T_Renaming_Declarations.R.Str (1);    -- Obj, Synonym, Global_All
   Scomp     : String    renames T_Renaming_Declarations.R.Str;        -- Obj, Synonym, Global_All
   Ren2_Call : Character renames Integer'Image (V) (1);                -- Call, Global_All
   Deref1    : String    renames Ptr1.all;                             -- Obj, Global_All
   Deref2    : Character renames Ptr1 (1);                             -- Obj, Global_All
   Deref3    : String    renames Ptr2.Str;                             -- Obj, Global_All
   Deref4    : Character renames T_Renaming_Declarations.Ptr2.Str (3); -- Obj, Global_All
   function Ren_F (I : Integer)  return Integer renames F;             -- Not_as_op, Synonym, Global_All
   function "+"   (L : Integer)  return Integer renames F;             -- As_op, Synonym, Global_All
   function "+"   (L : Float)    return Float   renames "-";           -- Not_identical_op, As_Op, Op, Global_All
   function "*"   (L, R : Float) return Float   renames Standard."*";  -- As_Op, Op, Global_All
   function Succ  (C : Integer)  return Integer renames Integer'Succ;  -- Not_as_op, Global_All

   procedure Proc3 renames Proc1;                                      -- As_proc_spec, Synonym, Global_All
   procedure Proc2 renames Proc1;                                      -- As_proc_body, Synonym, Global_All
   procedure Proc5 renames Proc3;                                      -- As_proc_spec, Synonym, Ren_Ren, Global_All

   package Other_Pack renames Pack;                                    -- Local_pack, Synonym, Global_All
   package Yet_Other_Pack renames Other_Pack;                          -- Local_pack, Synonym, Ren_Ren, Global_All
   package My_IO renames Ada.Text_IO;                                  -- Libr_Pack, Global_All
begin
   declare
      V : Integer renames T_Renaming_Declarations.V;                   -- Obj, Local_All

      function F (L, R : Integer) return Integer renames "+";          -- Not_identical_op, Op_not_as_op, Not_as_op, Op, Local_All
      EE  : exception renames E;                                       -- Exc, Local_All
      EEE : exception renames EE;                                      -- Exc, Synonym, Ren_Ren, Local_All

      generic procedure Ren_Gen renames Gen;                           -- Ren_Gen, Local_All
      generic procedure Ren_Ren_Gen renames Ren_Gen;                   -- Ren_Gen, Synonym, Ren_Ren, Local_All
      generic function  Ren_Genf renames Gen_F;                        -- Not_as_op, local all (generic function not checked)
      generic package IIO renames Ada.Text_IO.Integer_IO;              -- Ren_Gen, Libr_Pack, Local_All
   begin
      null;
   end;
end T_Renaming_declarations;
