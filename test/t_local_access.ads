package T_Local_Access is
   type A_Int   is access all Integer;
   type C_Int   is access constant Integer;
   type A_Proc  is access procedure;
   type A_Func  is access function return Integer;
   type A_PProc is access protected procedure;
   type A_PFunc is access protected function return Integer;
   
   V : aliased Integer;
   C : aliased constant Integer := 1;
   
   procedure Proc;
end T_Local_Access;
