separate (T_Uncheckable)
procedure Potentially_Blocking_Operations is
   protected Prot is
      procedure P;
   end Prot;
   protected body Prot is
      procedure P is
      begin
         Dyn_Ren_Proc;              -- Uncheckable
         Dyn_Proc.all;              -- Uncheckable
         Dispatch (Dyn_Tagged);     -- Uncheckable
      end P;
   end Prot;

   generic
      with function F return Integer;
      with procedure P;
   package Gen is
   end Gen;
   package body Gen is
      protected PO is
         procedure Proc;
      end PO;
      protected body PO is
         procedure Proc is
            I : Integer := F;     -- Uncheckable
         begin
            P;                    -- Uncheckable
         end Proc;
      end PO;
   end Gen;

begin
   null;
end Potentially_Blocking_Operations;
