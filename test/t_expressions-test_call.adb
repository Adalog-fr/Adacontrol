separate (T_Expressions)
procedure Test_Call is
   function F (X : Integer) return Integer is
   begin
      return X;
   end F;

   procedure P (X : Integer) is
   begin
      null;
   end;

   I : Integer;

   package Pack is
      type Orig is
         record
            I : Integer;
         end record;
      function Inh   return Orig;
      function Redef return Orig;

      type Der is new Orig;
      function Redef return Der;
   end Pack;
   package body Pack is
      function Inh   return Orig is
      begin
         return Orig'(I => 1);   -- Record_Aggregate
      end Inh;

      function Redef return Orig is
      begin
         return Orig'(I => 1);   -- Record_Aggregate
      end Redef;

      function Redef return Der is
      begin
         return Der'(I => 1);     -- Record_Aggregate
      end Redef;
   end Pack;
   use Pack;

   O : Orig;
   D : Der;
begin
   P (I);
   P (2);
   I := F(I);
   P (F (I));      -- Complex_Parameter
   P (I + 1);      -- Complex_Parameter
   P (F (I + 1));  -- Complex_Parameter (x2)

   O := Inh;
   O := Redef;
   D := Inh;       -- Inherited_Function_Call, record Inherited_Function_Call
   D := Redef;
end Test_Call;
