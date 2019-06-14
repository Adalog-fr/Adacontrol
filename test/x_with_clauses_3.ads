private with Ada.Strings.Unbounded, Ada.Text_IO; -- Multiple names, private with
limited with X_With_Clauses_3.Child1;            -- Limited with
limited private with X_With_Clauses_3.Child2;    -- Limited private with

package X_With_Clauses_3 is
   subtype F00 is X_With_Clauses_3.Child1.Foo;
private
   type S is new Ada.Strings.Unbounded.Unbounded_String;
   procedure Console_Log (Message : String) renames Ada.Text_IO.Put_Line;
   subtype B4R is X_With_Clauses_3.Child2.Bar;
end X_With_Clauses_3;
