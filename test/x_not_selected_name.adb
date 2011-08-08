package body X_Not_Selected_Name is
   package body Pack is
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
   end Pack;

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

end X_Not_Selected_Name;
