separate (T_Uncheckable)
procedure Duplicate_Initialization_Calls is
   procedure Out_Proc (X : out Integer; Y : in Integer) is
   begin
      X := Y;
   end Out_Proc;

   Tab : array (1 .. 10) of Integer;
   I : Integer;
begin
   Out_Proc (Tab (1), 1);
   Out_Proc (Tab (I), 2);    -- uncheckable
end Duplicate_Initialization_Calls;
