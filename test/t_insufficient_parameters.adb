procedure T_Insufficient_Parameters is
   type Enum1 is (A, B, C);
   type Enum2 is (A, B, C);

   procedure P (X, Y: Integer; Z : Boolean := False) is
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
   P (1, 2, True);                  -- Check
   P (1, 2, Z => True);             -- Search
   P (1,             2*5+4, False); -- Check
   P (Integer'First, 2*5+4, False); -- Search (Attribute is OK)
   P (I, 2, True);                  -- Search
   P (I, 2);                        -- OK

   Q (1, 2, A);                     -- Check
   Q (I, 2, A);                     -- OK
   R (1, 2, A);                     -- Search
   R (I, 2, A);                     -- OK
end T_Insufficient_Parameters;
