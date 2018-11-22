procedure X_Not_Selected_Name.Child is
   use X_Not_Selected_Name.Pack;
begin
   X_Not_Selected_Name.N := 1;
   X_Not_Selected_Name.N := 1;

   X_Not_Selected_Name.Pack.U := 1;
   X_Not_Selected_Name.Pack.U := 1;
   X_Not_Selected_Name.Pack.U := 1;

   X_Not_Selected_Name.C := 1;
   X_Not_Selected_Name.C := 1;

   X_Not_Selected_Name.F := 1;
   F := 1;
end X_Not_Selected_Name.Child;
