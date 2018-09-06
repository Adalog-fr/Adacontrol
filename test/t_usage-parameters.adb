separate (T_Usage)
procedure Parameters is
   procedure P1 (I  : in Integer;      -- procedure
                 O  : out Integer;
                 Io : in out Integer)
   is
   begin
      O  := I;
      Io := Io +1;
   end P1;

   procedure P2 (I  : in Integer;      -- procedure, unused_param
                 O  : out Integer;     -- Read_Out
                 Io : in out Integer)  -- Change_To_Out
   is
   begin
      Io := O + 1;
   end P2;

   procedure P3 (I  : in Integer;      -- procedure, Unused_Param
                 O  : out Integer;
                 Io : in out Integer)  -- Change_to_in
   is
   begin
      O := Io + 1;
   end P3;

   V1, V2 : Integer;
begin
   P2 (1, V1, Io => V2);
end Parameters;
