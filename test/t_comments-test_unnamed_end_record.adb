separate (T_comments)
procedure Test_Unnamed_End_Record is
   type Ok_1 is
      record
         I : Integer;
      end record; -- Ok_1

   type Ok_2 is
      record
         I : Integer;
      end record; -- oK_2

   type Bad_1 is
      record
         I : Integer;
      end record;

   type Bad_2 is
      record
         I : Integer;
      end record;       -- Bad comment

   type Bad_3 is
      record
         I : Integer;
      end record;       --
begin   -- Test_Unnamed_End_Record
   null;
end Test_Unnamed_End_Record;