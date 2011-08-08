separate (T_Array_Declarations)
procedure Dimensions is
   type C1 is array (1..3) of Integer;
   type C2 is array (1..3, boolean) of Integer;                                  -- search Dimensions
   type C3 is array (1 .. 3, Boolean, Integer range -1 .. 1) of Integer;         -- search Dimensions, search First
   type C4 is array (1 .. 3, Boolean, Integer range -1 .. 1, 1 .. 3) of Integer; -- check Dimensions, search First

   type U1 is array (Positive range <>) of Integer;                                                          -- Check Last
   type U2 is array (Positive range <>, Boolean range <>) of Integer;                                        -- search Dimensions, check Last
   type U3 is array (Positive range <>, Boolean range <>, Positive range <>) of Integer;                     -- search Dimensions, check Last x2
   type U4 is array (Positive range <>, Boolean range <>, Positive range <>, Positive range <>) of Integer;  -- check Dimensions, check Last x3
begin
   null;
end Dimensions;
