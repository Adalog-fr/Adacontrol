with X_Not_Selected_Name, X_Not_Selected_Name.Child, X_Not_Selected_Name_Gen;
procedure T_Not_Selected_Name is
   Other_N : Integer renames X_Not_Selected_Name.N;
   package Inst is new X_Not_Selected_Name_Gen;
begin
   declare
      use X_Not_Selected_Name;
      use X_Not_Selected_Name.Pack;                          -- Pack
      Yet_Another_N : Integer renames X_Not_Selected_Name.N; -- N
   begin
      X_Not_Selected_Name.N := 1;
      X_Not_Selected_Name.N := 1;                            -- N

      X_Not_Selected_Name.Pack.U := 1;
      X_Not_Selected_Name.Pack.U := 1;                       -- Pack
      X_Not_Selected_Name.Pack.U := 1;                            -- U

      X_Not_Selected_Name.C := 1;
      X_Not_Selected_Name.C := 1;                            -- C

      X_Not_Selected_Name.F := 1;
      X_Not_Selected_Name.F := 1;                            -- F
   end;

   Other_N := 1;                         -- Ok

   declare
      package Other_Inst renames Inst;
      use Other_Inst;
   begin
      Other_Inst.N := 1;
      T_Not_Selected_Name.Inst.N := 1;                            -- N
      Inst.G := 1;
      T_Not_Selected_Name.Inst.G := 1;                            -- G
   end;

end T_Not_Selected_Name;
