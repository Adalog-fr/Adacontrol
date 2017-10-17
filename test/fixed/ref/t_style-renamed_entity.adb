separate (T_Style)
procedure Renamed_Entity is
   I : Integer;
   function F return Integer is
   begin
      return 1;
   end F;

   package Pack is
      V : Integer;
   end Pack;

begin
   I := 1;        --OK
   Pack.V := F;  -- OK

   declare
      function RenF return Integer renames F;
      package RenPack renames Pack;
      RenI : Integer renames I;
      J : Integer;
   begin
      RenI := 1;                -- Renamed_Entity
      RenPack.V := RenF;           -- Renamed_Entity (x2)

      RenI := 1;             -- OK
      RenPack.V := RenF;     -- OK
   end;

   I := 1;        --OK
   Pack.V := F;  -- OK
end Renamed_Entity;
