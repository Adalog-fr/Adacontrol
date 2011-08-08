with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body T_local_instantiation is

   procedure P is
      procedure Free is
         new Ada.Unchecked_Deallocation (String, String_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Integer, Integer_Access);

      function To_Integer_Access is
         new Ada.Unchecked_Conversion (String_Access, Integer_Access);
   begin
      null;
   end P;

   package body Pack1 is
      procedure P is
         procedure Free is
            new Ada.Unchecked_Deallocation (String, String_Access);

         procedure Free is
            new Ada.Unchecked_Deallocation (Integer, Integer_Access);

         function To_Integer_Access is
            new Ada.Unchecked_Conversion (String_Access, Integer_Access);
      begin
         null;
      end P;
   end Pack1;

begin
   declare
      procedure Free is
         new Ada.Unchecked_Deallocation (String, String_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Integer, Integer_Access);
   begin
      null;
   end;
end T_local_instantiation;
