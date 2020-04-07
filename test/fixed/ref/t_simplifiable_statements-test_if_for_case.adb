separate (T_Simplifiable_Statements)
procedure Test_If_For_Case is
   type Enum is (A, B, C, D);
   E : Enum;
   F : Float;
   I : Integer;
   Alias : Integer renames I;
   J : Integer;
   S : String (1..3);

   Const : constant Integer := 1;
   subtype Sub is Integer range -5 .. 0;

   type T1 is array (Positive range <>) of Character;
   subtype T2 is T1 (1..10);
   V : String (T2'Range);

   type Mat is array (1..10, 1..10) of Integer;
   M : Mat;
begin
   --
   -- Replaceable by case:
   --

   if E = A then  -- if_for_case
      null;
   elsif E = B then
      null;
   end if;

   if E = Enum'First then  -- if_for_case
      null;
   elsif B = E then
      null;
   elsif E = Enum'Last then
      null;
   elsif E = Enum'Pred (Enum'Last) then
      null;
   end if;

   if Standard."=" (I, 1) then  -- if_for_case
      null;
   elsif Test_If_For_Case.I = 2 or I = 3 or Alias = 4 then
      null;
   elsif I >= 5 and I <= 10 then
      null;
   elsif I in 11 .. 20 then
      null;
   elsif I = Const * (3 + 5*Const) then
      null;
   elsif I in Sub then
      null;
   elsif I in V'Range then
      null;
   elsif Boolean'(I in M'Range (2)) then
      null;
   elsif ((I = (Integer((100)))) or else ((I)=Integer'(200))) then
      null;
   else
      I := 0;
   end if;

   --
   -- Not replaceable by case:
   --
   if S = "123" then
      null;
   elsif S = "456" then
      null;
   else
      I := 0;
   end if;

   if F = 1.0 then
      null;
   elsif F = 2.0 then
      null;
   elsif F = 3.0 then
      null;
   end if;

   if E = E then
      null;
   elsif E > E then
      null;
   else
      I := 0;
   end if;

   null;

   if I = Const then
      null;
   elsif I = Const + 2*I then
      null;
   end if;

   if I = 1 then
      null;
   elsif J = 2 then
      null;
   end if;

   if I = 1 and J = 1 then
      null;
   elsif J = 2 then
      null;
   end if;

   declare
      function "=" (L, R : Integer) return Boolean is
      begin
         return False;
      end "=";
      function "+" (L, R : Integer) return Integer is
      begin
         return 1;
      end "+";
  begin
      if I = 1 then
         null;
      elsif I = 2 then
         null;
      end if;

      if I < 10 then
         null;
      elsif I < 10+2 then
         null;
      end if;
   end;

   declare
      function F return String is
      begin
         return "ada";
      end F;

      S : constant String := F;
      I : Integer;
   begin
      if I > S'Last  then -- Not static
         null;
      elsif I = 2 then
         null;
      else
         I := 0;
      end if;
   end;
end Test_If_For_Case;
