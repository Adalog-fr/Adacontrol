with X_Unnecessary_Use_Clause, Ada.Strings.Unbounded;
separate (T_Unnecessary_Use_Clause)
procedure Sep is
   I : Natural := Index ("ABCD", "BC");
begin
   X_Unnecessary_Use_Clause (Ada.Strings.Unbounded.Null_Unbounded_String);
end Sep;
