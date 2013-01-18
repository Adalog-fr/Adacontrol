separate (T_Uncheckable)
procedure Object_Declarations is
   type Int is range 1 .. 10;

   type T is tagged null record;
   pragma Volatile (T'Class);     -- Uncheckable x2

   V1 : T;                        -- (false - : should be Volatile_No_Address)
   V2 : T;                        -- Address_Not_Volatile (false +)
   for V2'Address use V1'Address;
   V3 : T'Class := V1;            -- Volatile_No_Address (OK)
   pragma Volatile (V3);
   V4 : T'Class := V1;            -- OK
   pragma Volatile (V4);
   for V4'Address use V3'Address;
begin
   null;
end Object_Declarations;
