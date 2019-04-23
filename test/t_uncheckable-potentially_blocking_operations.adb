separate (T_Uncheckable)
procedure Potentially_Blocking_Operations is
   protected Prot1 is
      procedure P;
   end Prot1;
   protected body Prot1 is
      procedure P is
      begin
         Dyn_Ren_Proc;                                  -- Uncheckable
         Dyn_Proc.all;                                  -- Uncheckable
         Dispatch (Dyn_Tagged);                         -- Uncheckable
      end P;
   end Prot1;

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
            I : Integer := F;                           -- Uncheckable
         begin
            P;                                          -- Uncheckable
         end Proc;
      end PO;
   end Gen;

   -- Objects of limited class-wide type
   package Obj is
      type Non_Lim is tagged null record;
      type Lim is tagged limited null record;
      function F_Lim return Lim'Class;
      function F_Non_Lim return Non_Lim'Class;
   end Obj;

   package body Obj is
      function F_Lim return Lim'Class is (Lim'(null record));
      function F_Non_Lim return Non_Lim'Class is (Non_Lim'(null record));
   end Obj;

   protected Prot2 is
      procedure Block;
      procedure Non_Block;
   end Prot2;

   protected body Prot2 is
      procedure Block is
         Lim_Obj : Obj.Lim'Class := Obj.F_Lim;          -- Uncheckable
         Lim_Cst : constant Obj.Lim'Class := Obj.F_Lim; -- Uncheckable
      begin
         null;
      end Block;

      procedure Non_Block is
         Non_Lim_Obj : Obj.Non_Lim'Class := Obj.F_Non_Lim;
         Non_Lim_Cst : constant Obj.Non_Lim'Class := Obj.F_Non_Lim;
      begin
         null;
      end Non_Block;
   end Prot2;
begin
   null;
end Potentially_Blocking_Operations;
