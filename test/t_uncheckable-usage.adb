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
begin
   V := Elem;                           -- Uncheckable

   Ptr := V'Access;                     -- Uncheckable
   Ptr := C'Unchecked_Access;           -- Uncheckable
   if V'Address = Null_Address then     -- Uncheckable
      null;
   end if;
end Usage;
