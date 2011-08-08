with X_Not_Selected_Name, X_Not_Selected_Name.Child;
procedure T_Not_Selected_Name is
   use X_Not_Selected_Name;
   use Pack;
begin
   X_Not_Selected_Name.N := 1;
   N := 1;

   X_Not_Selected_Name.Pack.U := 1;
   Pack.U := 1;
   U := 1;

   X_Not_Selected_Name.C := 1;
   C := 1;

   X_Not_Selected_Name.F := 1;
   F := 1;

end T_Not_Selected_Name;
