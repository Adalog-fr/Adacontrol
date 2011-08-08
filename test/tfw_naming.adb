with Ada.Unchecked_Deallocation;
with Xfw_Pack;
pragma Elaborate_All (Xfw_Pack);

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
      xfw_pack.Proc_1;

end Tfw_naming;
