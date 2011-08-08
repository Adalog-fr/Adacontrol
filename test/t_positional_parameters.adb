procedure T_Positional_Parameters is
   type Enum1 is (A, B, C);
   type Enum2 is (A, B, C);

   procedure P (X, Y: Integer; Z : Boolean) is
   begin
      null;
   end;

   procedure Q (X, Y: Integer; Z : Enum1) is
   begin
      null;
   end;

   procedure R (X, Y: Integer; Z : Enum2) is
   begin
      null;
   end;

   I : Integer;
begin
   P (1, 2, True);                  -- Insufficient + Maximum
   P (1, 2, Z => True);             -- OK
   P (Integer'First, 2*5+4, False); -- Maximum (Attribute is OK)
   P (I, 2, True);                  -- Maximum
   P (Integer'First, 2*I+4, False); -- Maximum

   Q (1, 2, A);                     -- Insufficient + Maximum
   R (1, 2, A);                     -- Maximum
end T_Positional_Parameters;
