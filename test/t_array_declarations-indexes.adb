separate (T_Array_Declarations)
procedure Indexes is

   type Range0 is range 0 .. 5;
   type Range1 is range 1 .. 5;
   type Range2 is range 2 .. 5;

   type C10 is array (0 .. 5) of Integer;           -- search First, check Length max
   type C11 is array (1 .. 5) of Integer;           -- search Length max
   type C12 is array (2 .. 5) of Integer;           -- check First

   type C20 is array (Range0) of Integer;           -- search First, check Length max
   type C21 is array (Range1) of Integer;           -- search Length max
   type C22 is array (Range2) of Integer;           -- check First

   type C31 is array (Range0 range Range0'Succ (Range0'First) .. Range0'Last) of Integer; -- search Length max
   type C32 is array (Range0 range 2 .. Range0'Last) of Integer;                          -- check First

   type U0 is array (Range0 range <>) of Integer;   -- search First
   type U1 is array (Range1 range <>) of Integer;
   type U2 is array (Range2 range <>) of Integer;   -- check First

   subtype S0 is U0 (Range0'First .. Range0'Last);  -- search First, check Length max
   subtype S1 is U1 (Range1);                       -- search Length max
   subtype S2 is U2 (Range2'Range);                 -- check First

   V0 : array (0 .. 5) of Integer;                  -- search First, check Length max
   V1 : array (1 .. 5) of Integer;                  -- search Length max
   V2 : array (2 .. 5) of Integer;                  -- check First
   V3 : array (0 .. 1, 1 .. 4, 2 .. 10) of Integer; -- search First, check First, check Length max, search dimensions
   V4 : array (1 .. 0, 1 .. 1, 2 .. 10) of Integer; -- check Lenght min, search Length min, check First, check Length max, search dimensions

   type C40 is array (1 .. 1000) of Integer;        -- check Length max
   type C41 is array (1 .. 1001) of Integer;        -- check Last max, check Length max

begin
   null;
end Indexes;
