with X_Unnecessary_Use_Clause, Ada.Strings.Unbounded, Ada.Numerics.Complex_Types;
 -- Unused: Numerics.Complex_Types not used (no child)
separate (T_Unnecessary_Use_Clause)
procedure Sep is
   I : Natural := Index ("ABCD", "BC");
begin
   X_Unnecessary_Use_Clause.Put_Line (Ada.Strings.Unbounded.Null_Unbounded_String);
end Sep;
