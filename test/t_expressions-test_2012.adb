separate (T_Expressions)
procedure Test_2012 is
   Set : constant array (Natural range 1..3) of Boolean := (True, False, True);  -- Array_Aggregate, unqualified_aggregate
   S1  : constant String  := (if Set (1) then "True" else "False");              -- if
   B   : constant Boolean := (if Set (1) then Set (2));                          -- if, if_no_else
   S2  : constant String  := (if Set (1) then "One"                              -- if
                              elsif Set (2)then "Two"                            -- if_elsif
                              else "False");
   S3  : constant String := (case S1 (1) is                                      -- case
                                when 'A' .. 'Z' => "upper_letter",
                                when 'a' .. 'z' => "lower letter",
                                when others     => "not a letter");
begin
   if (for all I in Set'Range => Set (I)) then                                   -- For_All
        null;
   elsif (for Some I in S1'Range => S1 (I) = 'a') then                           -- For_Some
        null;
   end if;
end Test_2012;
