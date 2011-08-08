procedure T_Array_Declarations is

   -- First
   type Range0 is range 0 .. 5;
   type Range1 is range 1 .. 5;
   type Range2 is range 2 .. 5;

   type C10 is array (0 .. 5) of Integer;
   type C11 is array (1 .. 5) of Integer;
   type C12 is array (2 .. 5) of Integer;

   type C20 is array (Range0) of Integer;
   type C21 is array (Range1) of Integer;
   type C22 is array (Range2) of Integer;

   type C31 is array (Range0 range Range0'Succ (Range0'First) .. Range0'Last) of Integer;
   type C32 is array (Range0 range 2 .. Range0'Last) of Integer;

   type U0 is array (Range0 range <>) of Integer;
   type U1 is array (Range1 range <>) of Integer;
   type U2 is array (Range2 range <>) of Integer;

   subtype S0 is U0 (Range0'First .. Range0'Last);
   subtype S1 is U1 (Range1);
   subtype S2 is U2 (Range2'Range);

   V0 : array (0 .. 5) of Integer;
   V1 : array (1 .. 5) of Integer;
   V2 : array (2 .. 5) of Integer;

   V3 : array (0..1, 1..4, 2..10) of Integer;

   -- Max_Length
begin
   null;
end T_Array_Declarations;
