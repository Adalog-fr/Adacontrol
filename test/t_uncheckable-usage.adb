with System; use System;
separate (T_Uncheckable)
procedure Usage is

   V : aliased Integer;                 -- Usage
   C : aliased constant Integer := 1;   -- Usage

   type Acc is access constant Integer;
   Ptr  : Acc;                           -- Usage
   Elem : Integer renames Ptr.all;

   generic
      X : in Integer;
   procedure Gen;
   procedure Gen is
   begin
     null;-- V := X;
   end Gen;

   procedure Inst is new Gen (Elem);    -- Uncheckable

   package Pack is
      type TT is tagged null record;
      procedure Dispatch (X : in TT);
   end Pack;

   package body Pack is
      K : TT;                           -- Usage

      procedure Q is
      begin
         Dispatch (TT'Class (K));       -- Uncheckable
      end Q;

      procedure Dispatch (X : in TT) is
      begin
         null;
      end Dispatch;
   end Pack;

begin
   V := Elem;                           -- Uncheckable

   Ptr := V'Access;                     -- Uncheckable
   Ptr := C'Unchecked_Access;           -- Uncheckable
   if V'Address = Null_Address then     -- Uncheckable
      null;
   end if;
end Usage;
