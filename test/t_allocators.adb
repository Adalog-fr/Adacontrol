procedure T_allocators is
   procedure Anonymous is separate;

   type Enum is (One, Two, Three);
   type Acc_Enum is access Enum;

   type Acc1   is access Integer;
   type Acc1c1 is access Positive;
   type Acc1c2 is access Integer range 1..10;
   type Acc2   is access String;

   type Int   is range 1..10;
   type Acc3  is access Int;
   type Acc3c is access T_Allocators.Int'Base;

   subtype Str10 is Wide_String (1 .. 10);
   type Acc4  is access Wide_String;
   type Acc4c is access Str10;

   type Arr is array (1 .. 10) of Integer;
   type Acc_Arr is access Arr;
   type Der is new Arr;
   type Acc_Der is access Der;

   package Pack is
      type Priv is private;
      type Acc_Priv is access Priv;
   private
      type Priv is new Arr;
   end Pack;
   use Pack;

   type Rec is null record;
   type Acc_Rec is access Rec;

   task type T1;
   task body T1 is
   begin
      null;
   end T1;
   type Acc_T1 is access T1;

   task type T2;
   task body T2 is
   begin
      null;
   end T2;
   type Acc_T2 is access T2;

   protected type P1 is
      procedure Proc;
   end P1;
   protected body P1 is
      procedure Proc is
      begin
         null;
      end Proc;
   end P1;
   type Acc_P1 is access P1;

   protected type P2 is
      procedure Proc;
   end P2;
   protected body P2 is
      procedure Proc is
      begin
         null;
      end Proc;
   end P2;
   type Acc_P2 is access P2;

   type Tag1 is tagged null record;
   subtype Tag_Class is Tag1'Class;
   type Acc_Tag1 is access Tag1;
   type Acc_Tag_Class is access Tag_Class;

   generic
   package Gen is
      type Gen_T is digits 5;
   end Gen;
   package Inst is new Gen;
   type Acc_Gen_T is access Inst.Gen_T;

   VE  : Acc_Enum;
   V1  : Acc1;
   V1_2: Acc1c2;
   V2  : Acc2;
   V3  : Acc_T1;
   V4  : Acc_T2;
   V5  : Acc_P1;
   V6  : Acc_P2;
   V7  : Acc_Tag1;
   V8  : Acc_Tag_Class;
   V9  : Acc_Gen_T;
   V10 : Acc1c1;
   V11 : Acc3;
   V12 : Acc3c;
   V13 : Acc4;
   V14 : Acc4c;
   V15 : Acc_Arr;
   V16 : Acc_Der;
   V17 : Acc_Priv;
   V18 : Acc_Rec;
   V19 : access Wide_Wide_String;
begin
   VE := new Enum;                             -- All_Enum
   V1 := new Integer;                          -- ALL
   V1 := new Integer'(1);                      -- ALL
   V1 := new Standard.Integer'Base'(1);        -- Inconsistent
   V1_2 := new Integer;                        -- Inconsistent
   V1_2 := new Integer'(4);                    -- Inconsistent
   V2 := new String (1..3);                    -- Allocator for String
   V2 := new String'("Hello world!");          -- Allocator for String
   V3 := new T1;                               -- Specific_Task
   V4 := new T2;                               -- All_Tasks
   V5 := new P1;                               -- Specific_Prot
   V6 := new P2;                               -- All_Prot
   V7 := new Tag1;                             -- Tagged
   V7 := new Tag1'(null record);               -- Tagged
   V8 := new Tag1;                             -- Tagged
   V8 := new Tag1'(V7.all);                    -- Tagged
   V8 := new Tag1'Class'(Tag1'Class(V7.all));  -- Class1
   V8 := new Tag_Class'(Tag_Class (V7.all));   -- Class1
   V9 := new Inst.Gen_T;                       -- From_Gen

   V1  := new Integer'(3);                     -- ALL
   V1  := new Positive'(3);                    -- Inconsistent
   V10 := new Integer'(3);                     -- Inconsistent
   V10 := new Positive'(3);                    -- ALL

   V11 := new Int'(1);                         -- ALL
   V11 := new Int'Base'(1);                    -- Inconsistent
   V12 := new Int'(1);                         -- All
   V12 := new Int'Base'(1);                    -- All

   V13 := new Str10;                           -- All_Arrays
   V13 := new Wide_String (1 .. 10);           -- All_Arrays
   V13 := new Wide_String (1 .. 20);           -- All_Arrays

   V14 := new Str10;                           -- All_Arrays
   V14 := new Wide_String (1 .. 10);           -- Inconsistent
   V14 := new Wide_String (1 .. 20);           -- Inconsistent

   V15 := new Arr;                             -- All_Arrays
   V16 := new Der;                             -- All_Arrays
   V17 := new Priv;                            -- All_Arrays
   V18 := new Rec;                             -- All_Records

   V19 := new Wide_Wide_String'("ABCD");       -- OK, not Wide_Wide_String
   V19 := new Wide_Wide_String (1..100);       -- OK, not Wide_Wide_String
end T_allocators;
