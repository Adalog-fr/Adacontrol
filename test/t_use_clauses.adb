with Ada.Text_IO, System, Ada.Strings;
procedure T_use_clauses is
   use Ada.Text_IO;
   use Ada.Text_IO, Ada.Strings;
   use Ada.Text_IO, Ada.Strings, System;
begin
   null;
end T_use_clauses;

