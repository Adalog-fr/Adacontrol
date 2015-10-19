package T_Naming_Convention is
   L_X1 : Integer;
   G_X1 : Integer;

   procedure G_Proc;

   package Inner is
      L_X2 : Integer;
      G_X2 : Integer;

      procedure G_Proc;
   end Inner;

   type Incomplete_T;     -- Ticket #0000041
   type T_Incomplete_Access_Sp is access procedure (C_Param : Incomplete_T);
   type Incomplete_T is new Integer;
end T_Naming_Convention;
