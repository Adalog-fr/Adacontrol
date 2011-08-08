with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with X2_Instantiations;
package X_Instantiations is

   type String_Access is access String;
   type Integer_Access is access Integer;

   procedure Free is   -- T1, T4
      new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Free is   -- T1, T3
      new Ada.Unchecked_Deallocation (Integer, Integer_Access);

   function To_Integer_Access is
      new Ada.Unchecked_Conversion (String_Access, Integer_Access);

   procedure P;

   package Pack1 is
      procedure Free is   -- T1, T4
	 new Ada.Unchecked_Deallocation (String, String_Access);

      procedure Free is   -- T1, T3
	 new Ada.Unchecked_Deallocation (Integer, Integer_Access);

      function To_Integer_Access is
	 new Ada.Unchecked_Conversion (String_Access, Integer_Access);

      procedure P;
   end Pack1;

end X_Instantiations;
