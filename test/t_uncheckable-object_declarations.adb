separate (T_Uncheckable)
procedure Object_Declarations is
   type Int is range 1 .. 10;

   type T is tagged null record;
   pragma Volatile (T'Class);     -- Uncheckable

   V1 : T;
   V2 : T'Class := V1;            -- Uncheckable
   pragma Volatile (V2);
begin
   null;
end Object_Declarations;
