procedure T_Non_Static_Constraints is
   type D (X : Integer) is null record;

   V : Integer;

   type Tab1 is array (Integer range <>)    of Integer;
   type Tab2 is array (Integer range 1..10) of Integer;
   type Tab3 is array (Integer range 1..V)  of Integer; -- should trigger

   X : D(V);                                            -- should trigger
   Y : D(1);

   T1 : Tab1 (1..10);
   T2 : array (1..10) of Integer;
   T3 : Tab1 (1..V);                                    -- should trigger
   T4 : array (1..V) of Integer;                        -- should trigger

   subtype Machin is Integer range 1..V;                -- not index or discr. constraint

begin
   null;
end T_Non_Static_Constraints;
