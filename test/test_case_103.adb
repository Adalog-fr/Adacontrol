with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

procedure Test_Case_103 is

   type String_Access is access String;
   type String_Access_2 is access String;

   type Integer_Access is access Integer;
   type Natural_Access is access Natural;

   -- T2 never matched

   procedure Free_1 is             -- T1, T4, T5
      new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Free_2 is             -- T1, T4
      new Ada.Unchecked_Deallocation (String, String_Access_2);

   procedure Free_3 is             -- T1, T3
      new Ada.Unchecked_Deallocation (Standard.Integer, Integer_Access);

   procedure Free_4 is             -- T1, T3
      new Ada.Unchecked_Deallocation (Standard.Natural, Natural_Access);

   function To_Integer_Access is   -- T6
      new Ada.Unchecked_Conversion (String_Access, Integer_Access);

begin
   null;
end Test_Case_103;
