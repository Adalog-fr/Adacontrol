procedure Tfw_Gpr is
   B : Boolean;
begin
<<Top>>   B := B and B;
   goto Top;
end;
