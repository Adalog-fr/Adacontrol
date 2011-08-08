separate (T_Object_Declarations)
procedure Test_Volatile_Address is
   type T is range 1 .. 10;
   pragma Volatile (T);
   type D is new T;
   subtype S is D;
   X1 : T;                   -- volatile_no_address
   X2 : D;                   -- volatile_no_address
   X3 : S;                   -- volatile_no_address
   Y1, Y2, Y3 : Integer;     -- volatile_no_address (x2)
   pragma Volatile (Y2);
   pragma Volatile (Y3);

   Z1, Z2 : Integer;
   pragma Volatile (Z1);
   pragma Volatile (Z2);
   for Z1'Address use Y1'Address;
   for Z2 use at Y2'Address;

   Z3, Z4 : Integer;         -- Address_no_volatile (x2)
   for Z3'Address use Y1'Address;
   for Z4 use at Y2'Address;

begin
   null;
end Test_Volatile_Address;
