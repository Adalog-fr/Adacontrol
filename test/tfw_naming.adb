with Ada.Unchecked_Deallocation;
with Xfw_Naming;
pragma Elaborate_All (Xfw_Naming);

procedure Tfw_Naming is

   type String_Access is access String;
   type Integer_Access is access Integer;

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);

   procedure P is
   begin
      null;
   end P;

   procedure P (I : in Integer) is
   begin
      null;
   end P;

   procedure Q is
   begin
      null;
   end Q;

   procedure Q (I : in Integer) is
   begin
      null;
   end Q;

   X : Integer'Base;

   type T is tagged null record;  -- Mantis 0000007
   procedure P (X : access T'Class'Class'Class) is
   begin
      null;
   end;

begin
   X := Integer'Base'First;

exception
   when Constraint_Error =>
      null;

   when Program_Error =>
      Q (1);
      Q;
      P(1);

   when others =>
      xfw_Naming.Proc_1;

end Tfw_naming;
