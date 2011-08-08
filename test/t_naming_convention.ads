package T_Naming_Convention is
   L_X1 : Integer;
   G_X1 : Integer;

   procedure L_Proc;

   package Inner is
      L_X2 : Integer;
      G_X2 : Integer;

      procedure L_Proc;
   end Inner;

end T_Naming_Convention;
