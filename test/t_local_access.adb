package body T_Local_Access is
   V1 : A_Int;
   V2 : C_Int;
   V3 : A_Proc;
   V4 : A_Func;
   V5 : A_PProc;
   V6 : A_PFunc;
   V7 : A_TT;
   V8 : A_PT;

   procedure Proc is
   begin
      null;
   end;

   function Func return Integer is
   begin
      return 1;
   end;

   task body TT is
   begin
      V7 := TT'Unchecked_Access;  -- Possible Local_Access variable
   end;

   protected body PT is
      procedure P is
      begin
         V8 := PT'Unchecked_Access;  -- Possible Local_Access variable
      end;
   end PT;

   procedure Local is
      V : aliased Integer;
      C : aliased constant Integer := 1;

      T : TT;
      P : PT;

      procedure Proc is
      begin
	 null;
      end;

      function Func return Integer is
      begin
	 return 1;
      end;
      function Other_Func return Integer renames Func;

      protected type T_Sema is
	 entry P;
	 procedure V;
	 function Length return Integer;
      end T_Sema;
      protected body T_Sema is
	 entry P when True is
	 begin
	    null;
	 end P;
	 procedure V is
	 begin
	    null;
	 end V;
	 function Length return Integer is
	 begin
	    return 1;
	 end Length;
      end T_Sema;

      Sema : T_Sema;

      task type TT_Local;
      Ptr_TT_Local : access TT_Local;
      task body TT_Local is
      begin
         Ptr_TT_Local := TT_Local'Unchecked_Access;  -- Local_Access variable
      end;

      protected type PT_Local is
         procedure P;
      end PT_Local;
      Ptr_PT_Local : access PT_Local;
      protected body PT_Local is
         procedure P is
         begin
            Ptr_PT_Local := PT_Local'Unchecked_Access;  -- Local_Access variable
         end;
      end PT_Local;

   begin
      V1 := V'Unchecked_Access;              -- Local_Access variable
      V2 := C'Unchecked_Access;              -- Local_Access constant
      V3 := Proc'Unrestricted_Access;        -- Local_Access procedure
      V4 := Func'Unrestricted_Access;        -- Local_Access function
      V4 := Other_Func'Unrestricted_Access;  -- Local_Access function
      V5 := Sema.V'Unrestricted_Access;      -- Local_Access protected procedure
      V6 := Sema.Length'Unrestricted_Access; -- Local_Access protected function
   end Local;

begin
   V1 := V'Access;    -- OK
   V2 := C'Access;    -- OK
   V3 := Proc'Access; -- OK
   V4 := Func'Access; -- OK
end T_Local_Access;
