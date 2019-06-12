with Ada.Text_IO, System, Ada.Strings, Ada.Numerics.Complex_Types;
use          Ada.Text_IO;                           -- Found: Global Ada.Strings, Check: Global System, OK: Text_IO (exempted)
use     type Ada.Numerics.Complex_Types.Complex;  -- Found: Global type File_type, OK: Complex (exempted)
 -- Foundx2: Global all type Truncation, Complex
procedure T_use_clauses is
   use Ada.Text_IO;
   use Ada.Text_IO;                                         -- Found: local Ada.Strings
   use Ada.Text_IO;                                 -- Found: local Ada.Strings, Error: System

                                          -- Found: type local Alignment

                                        -- Check: all_type_local Any_Priority
   use all type Ada.Text_IO.Count;                                       -- OK (exempted)
begin
   null;
end T_use_clauses;
