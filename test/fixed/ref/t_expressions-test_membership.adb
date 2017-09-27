separate (T_Expressions)
procedure Test_Membership is
   type T is range 1 .. 100;
   subtype S1 is T;
   subtype S2 is T range 1 .. 10;

   V : T;
   V1 : S1;
   V2 : S2;

   type Rec (D : S1) is
      record
         C1 : S1;
         C2 : S2;
      end record;
   R : Rec (5);

   procedure P (X : S1) is
   begin
      if   (X in T)          -- Unparenthesized_mixed, in, static_membership
        or (X   not in S2)   -- or, or_boolean, unparenthesized_mixed, not_in
      then
         null;
      end if;
   end P;

   type TTab is array (T range <>) of Integer;
   Tab1 : TTab (5 .. S2'Last);
   Tab2 : array (S2) of Integer;
   VT1  : T range Tab1'Range;
   VT2  : T range Tab2'Range;

begin
   if   (V in T)          -- Unparenthesized_mixed, in, static_membership
     or (V in S1)         -- or, or_boolean, Unparenthesized_mixed, in, static_membership
     or (V not in S2)     -- or, or_boolean, Unparenthesized_mixed, not_in
   then
      null;
   end if;

   if   (V1 in T)          -- Unparenthesized_mixed, in, static_membership
     or (V1   not in S1)   -- or, or_boolean, Unparenthesized_mixed, not_in, static_membership
     or (R.D      in S1)   -- or, or_boolean, Unparenthesized_mixed, in, static_membership
     or (R.C1     in S1)   -- or, or_boolean, Unparenthesized_mixed, in, static_membership
     or (V1 in S2)         -- or, or_boolean, Unparenthesized_mixed, in
   then
      null;
   end if;

   if   (V2 not in T)      -- Unparenthesized_mixed, not_in,
     or (V2 in S1)         -- or, or_boolean, Unparenthesized_mixed, in
     or (V2 in S2)         -- or, or_boolean, Unparenthesized_mixed, in, static_membership
     or (R.C2 not in S2)   -- or, or_boolean, Unparenthesized_mixed, not_in, static_membership
   then
      null;
   end if;

   if V in 1 .. 100 then      -- in, static_membership
      null;
   end if;

   if (VT1 in Tab1'Range)       -- Unparenthesized_mixed, in, static_membership
     or (VT1 in Tab2'Range)     -- or, or_boolean, Unparenthesized_mixed, in
   then
      null;
   end if;

   if (VT2 in Tab2'Range)       -- Unparenthesized_mixed, in, static_membership
     or (VT2 not in Tab1'Range) -- or, or_boolean, Unparenthesized_mixed, not_in
   then
      null;
   end if;
end Test_Membership;
