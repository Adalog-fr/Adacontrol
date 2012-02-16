pragma Ada_2005;
procedure T_allocators is
   type Acc1  is access Integer;
   type Acc1c is access Positive;
   type Acc2  is access String;

   type Int   is range 1..10;
   type Acc3  is access Int;
   type Acc3c is access T_Allocators.Int'Base;

   type Acc4 is access Wide_String;
   subtype Str10 is Wide_String (1 .. 10);

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

   V1  : Acc1;
   V2  : Acc2;
   V3  : Acc_T1;
   V4  : Acc_T2;
   V5  : Acc_P1;
   V6  : Acc_P2;
   V7  : Acc_Tag1;
   V8  : Acc_Tag_Class;
   V9  : Acc_Gen_T;
   V10 : Acc1c;
   V11 : Acc3;
   V12 : Acc3c;
   V13 : Acc4;
   V14 : Acc_Arr;
   V15 : Acc_Der;
   V16 : Acc_Priv;
   V17 : Acc_Rec;
   V18 : access Wide_Wide_String;
begin
   V1 := new Integer;
   V1 := new Integer'(1);
   V1 := new Standard.Integer'Base'(1);
   V2 := new String (1..3);
   V2 := new String'("Hello world!");
   V3 := new T1;
   V4 := new T2;
   V5 := new P1;
   V6 := new P2;
   V7 := new Tag1;
   V7 := new Tag1'(null record);
   V8 := new Tag1;
   V8 := new Tag1'(V7.all);
   V8 := new Tag1'Class'(Tag1'Class(V7.all));
   V8 := new Tag_Class'(Tag_Class (V7.all));
   V9 := new Inst.Gen_T;

   V1  := new Integer'(3);
   V1  := new Positive'(3);
   V10 := new Integer'(3);
   V10 := new Positive'(3);

   V11 := new Int'(1);
   V11 := new Int'Base'(1);
   V12 := new Int'(1);
   V12 := new Int'Base'(1);

   V13 := new Str10;

   V14 := new Arr;
   V15 := new Der;
   V16 := new Priv;
   V17 := new Rec;

   V18 := new Wide_Wide_String'("ABCD");
end T_allocators;
