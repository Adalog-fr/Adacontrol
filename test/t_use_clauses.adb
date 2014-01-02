with Ada.Text_IO, System, Ada.Strings, Ada.Numerics.Complex_Types;
use Ada.Text_IO, Ada.Strings, System;                                -- Found: Global Ada.Strings, Global System
use type Ada.Text_IO.File_Type, Ada.Numerics.Complex_Types.Complex;  -- Found: Global type File_type
procedure T_use_clauses is
   use Ada.Text_IO;
   use Ada.Text_IO, Ada.Strings;                                     -- Found: local Ada.Strings
   use Ada.Text_IO, Ada.Strings, System;                             -- Found: local Ada.Strings, Error: System
   use type Ada.Strings.Alignment;                                   -- Found: type local Alignment
begin
   null;
end T_use_clauses;

