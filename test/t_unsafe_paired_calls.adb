procedure T_Unsafe_Paired_Calls is
   procedure Not_A_Lock is begin null; end;

   procedure P is begin null; end;
   procedure V is begin null; end;

   procedure PP renames P;
   procedure VV renames V;

   procedure P (C : Character; X : Integer) is begin null; end;
   procedure V (C : Character; X : Integer := 1) is begin null; end;

   type Acc is access Integer;
   procedure P (S : in Acc) is begin null; end;
   procedure V (S : in Acc) is begin null; end;

   type Sema is null record;
   procedure P (S : in out Sema) is begin null; end;
   procedure V (S : in out Sema) is begin null; end;

   procedure MultP1 is begin null; end;
   procedure MultV1 is begin null; end;
   procedure MultP2 is begin null; end;
   procedure MultV2 is begin null; end;

   procedure Simple  is separate;
   procedure Nesting is separate;
   procedure Breaks  is separate;
begin
   null;
end T_Unsafe_Paired_Calls;
