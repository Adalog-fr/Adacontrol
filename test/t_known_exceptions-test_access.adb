separate (T_Known_Exceptions)
procedure Test_Access is
begin
   -- Explicit dereference
   declare
      type Int_Ptr is access all Integer;
      I : aliased Integer;

      IP1, IP2 : Int_Ptr;
      V        : Integer renames IP1.all;   -- Access_Error
   begin
      IP1.all := 3;                         -- Access_Error

      IP1 := I'Access;
      IP1.all := 3;
      IP2 := IP1;
      IP2.all := 3;

      IP1 := null;
      IP1.all := 3;                         -- Access_Error
      IP2 := IP1;
      IP2.all := 3;                         -- Access_Error

      IP1 := new Integer'(3);
      IP1.all := 5;
      IP2 := IP1;
      IP2.all := 3;
   end;

   -- Implicit dereference, array
   declare
      type Str_Ptr is access all String;
      V : aliased String := "Adacontrol";

      SP1, SP2 : Str_Ptr;
      C        : Character renames SP1 (1); -- Access_Error
   begin
      SP1 (1) := SP2 (1);                   -- Access_Error x2

      SP1 := V'Access;
      SP2 := SP1;
      SP2 (1) := SP1 (2);

      SP1 := new String'("Adacontrol");
      SP2 := SP1;
      SP2 (1) := SP1 (2);

      SP1 := null;
      SP2 (1) := SP1 (2);                   -- Access_Error
   end;

   -- Implicit dereference, record
   declare
      type Rec is
         record
            Comp : Integer;
         end record;
      type Rec_Ptr is access all Rec;
      V : aliased Rec := (Comp => 0);

      SP1, SP2 : Rec_Ptr;
      I        : Integer renames SP1.Comp;  -- Access_Error
   begin
      SP1.Comp := SP2.Comp;                 -- Access_Error x2

      SP1 := V'Access;
      SP2 := SP1;
      SP1.Comp := SP2.Comp;

      SP1 := new Rec'(Comp => 0);
      SP2 := SP1;
      SP1.Comp := SP2.Comp;

      SP1 := null;
      SP1.Comp := SP2.Comp;                 -- Access_Error
   end;

end Test_Access;
