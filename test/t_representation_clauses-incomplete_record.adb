separate (T_Representation_Clauses)
procedure Incomplete_Record is
   type T1 (D : Boolean) is
      record
         I1 : Integer;
         I2 : Integer;
         case D is
            when True =>
               I3 : Integer;
            when False =>
               I4 : Integer;
         end case;
      end record;
   for T1 use                    -- record, no clause for I2, D, I4
      record
         I1 at 0 range 0 .. 31;
         I3 at 4 range 0 .. 31;
      end record;
   for T1'Bit_Order use Low_Order_First;
begin
   null;
end Incomplete_Record;
