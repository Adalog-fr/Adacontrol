with Ada.Text_IO, System, Ada.Strings, Ada.Numerics.Complex_Types;
use          Ada.Text_IO, Ada.Strings, System;                           -- Found: Global Ada.Strings, Check: Global System, OK: Text_IO (exempted)
use     type Ada.Text_IO.File_Type, Ada.Numerics.Complex_Types.Complex;  -- Found: Global type File_type, OK: Complex (exempted)
use all type Ada.Strings.Truncation, Ada.Numerics.Complex_Types.Complex; -- Foundx2: Global all type Truncation, Complex
procedure T_use_clauses is
   use Ada.Text_IO;
   use Ada.Text_IO, Ada.Strings;                                         -- Found: local Ada.Strings
   use Ada.Text_IO, Ada.Strings, System;                                 -- Found: local Ada.Strings, Error: System

   use type Ada.Strings.Alignment;                                       -- Found: type local Alignment

   use all type System.Any_Priority;                                     -- Check: all_type_local Any_Priority
   use all type Ada.Text_IO.Count;                                       -- OK (exempted)
begin
   null;
end T_use_clauses;
