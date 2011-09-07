-- Asis
with
  Asis;

package Rules.Local_Access is
   pragma Elaborate_Body;

   Rule_Id : constant Wide_String := "LOCAL_ACCESS";

   procedure Process_Attribute (Attr: Asis.Expression);
end Rules.Local_Access;
