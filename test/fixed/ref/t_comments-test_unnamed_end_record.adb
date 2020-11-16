separate (T_comments)
procedure Test_Unnamed_End_Record is
   -- Regular OK case
   type Ok_1 is
      record
         I : Integer;
      end record; -- Ok_1

   -- Regular case, different casing, more text
   type Ok_2 is
      record
         I : Integer;
      end record; -- oK_2 record

   -- No end record:
   type Ok_3 is null record;
   type Ok_4 is tagged null record;

   -- Count: No name, only one line
   type Optional_1 is record null; end record;

   -- Count: No name, only one line, tagged type
   type Optional_2 is tagged record null; end record;

   -- Found, count: 4 lines (semi-colon far away)
   type Bad_1 is
      record
         I : Integer;
      end record   ;  -- Bad_1

   -- Check, count: bad ending comment (although 4 lines)
   type Bad_2 is
      record
         I : Integer;
      end record;       -- Bad_2

   -- Found, count: empty comment, 4 lines
   type Bad_3 is
      record
         I : Integer;
      end record;       -- Bad_3

   -- Check, count: 5 lines
   type Bad_4 is
      record
         I : Integer;
         J : Integer;
      end record;  -- Bad_4

   -- Found, count: tagged type
   type Bad_5 is tagged
      record
         I : Integer;
      end record;  -- Bad_5

   -- Check, count: type extension
   type Bad_6 is new Bad_5 with
      record
         J : Integer;
         K : Integer;
      end record;  -- Bad_6

begin   -- Test_Unnamed_End_Record
   null;
end Test_Unnamed_End_Record;
