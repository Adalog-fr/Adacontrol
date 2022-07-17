with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body X_Instantiations is

   procedure P is
      procedure Free is   -- T1, T4, local_instantiation
         new Ada.Unchecked_Deallocation (String, String_Access);

      procedure Free is   -- T1, T3, local_instantiation
         new Ada.Unchecked_Deallocation (Integer, Integer_Access);

      function To_Integer_Access is  -- local_instantiation
         new Ada.Unchecked_Conversion (String_Access, Integer_Access);
   begin
      null;
   end P;

   package body Pack1 is
      procedure P is
         procedure Free is     -- T1, T4, local_instantiation
            new Ada.Unchecked_Deallocation (String, String_Access);

         procedure Free is     -- T1, T3, local_instantiation
            new Ada.Unchecked_Deallocation (Integer, Integer_Access);

         function To_Integer_Access is -- local_instantiation
            new Ada.Unchecked_Conversion (String_Access, Integer_Access);
      begin
         null;
      end P;
   end Pack1;

begin
   declare
      procedure Free is  -- T1, T4, local_instantiation, block_instantiation
         new Ada.Unchecked_Deallocation (String, String_Access);

      procedure Free is  -- T1, T3, local_instantiation, block_instantiation
         new Ada.Unchecked_Deallocation (Integer, Integer_Access);
   begin
      null;
   end;
end X_Instantiations;
