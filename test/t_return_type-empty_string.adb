-- A_Function_Body_Subunit
-- returning an unconstrained type
separate (T_Return_Type)
function Empty_String return String is   -- unconstrained array
begin
   return "";
end Empty_String;
