package body Framework.String_Set is
   Dummy : constant Null_Record := (null Record);

   ---------
   -- Add --
   ---------

   procedure Add (To : in out Set; Key : in Wide_String) is
   begin
      Add (To, To_Unbounded_Wide_String (Key), Dummy);
   end Add;

   ------------
   -- Delete --
   ------------

   procedure Delete (From : in out Set; Key : in Wide_String) is
   begin
      Delete (From, To_Unbounded_Wide_String (Key));
   end Delete;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (The_Set : in Set) return Boolean is
   begin
      return Null_Map.Is_Empty (Null_Map.Map (The_Set));
   end Is_Empty;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present (Within : in Set; Key : in Wide_String) return Boolean is
   begin
      return Is_Present (Within, To_Unbounded_Wide_String (Key));
   end Is_Present;

   -------------
   -- Balance --
   -------------

   procedure Balance (The_Set : in out Set) is
   begin
      Null_Map.Balance (Null_Map.Map (The_Set));
   end Balance;

   -----------
   -- Clear --
   -----------

   procedure Clear (The_Set : in out Set) is
   begin
      Null_Map.Clear (Null_Map.Map (The_Set));
   end Clear;

end Framework.String_Set;
