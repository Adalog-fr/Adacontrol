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
   Ptr: access function (X : Integer) return Integer := F'Access;

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
   I := F(I);      -- range function_call, function_call
   P (F (I));      -- Complex_Parameter, range function_call, function_call
   P (I + 1);      -- Complex_Parameter
   P (F (I + 1));  -- Complex_Parameter (x2), range function_call, function_call

   O := Inh;       -- Function_Call
   O := Redef;     -- Function_Call
   D := Inh;       -- Function_Call, Inherited_Function_Call, record Inherited_Function_Call
   D := Redef;     -- Function_Call

   I := Ptr (1);   -- range_function_call, Function_Call, Dynamic_Function_Call, implicit_dereference

   -- Dispatching
   declare
      package Object is
         type Instance is tagged null record;
         subtype Class is Instance'Class;

         function F1 (X : Instance) return Integer;
         function F2 return Instance;
      end Object;
      package body Object is
         function F1 (X : Instance) return Integer is
         begin
            return F1 (Instance'Class (X));    -- range function_call, function_call, dispatching_function_call, redispatching_function_call, type_conversion
         end;
         function F2 return Instance is
         begin
            return Instance'(null record);     -- record_aggregate
         end;
      end Object;
      use Object;

      Obj : Object.Instance;
      Val : Integer := F1 (Class (Obj));       -- range function_call, function_call, dispatching_function_call, type_conversion
   begin
      null;
   end;
   end Test_Call;
