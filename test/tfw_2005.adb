procedure Tfw_2005 is
   -- Only for stress test.
   -- Does not test anything by itself (hence considered tfw),
   -- but contains a little digest of the most error-prone functionalities
   -- of Ada 2005, to make sure new rules are not trapped by them.

   X : access Integer;                       -- Variable of anonymous access type
   Tab : array (1..10) of access Integer;    -- Anonymous array of anonymous access type
   type Rec is
      record
         F : access Integer'Base'Base;       -- Component of anonymous access type, several 'Base
                                             -- Several 'Base were already allowed before 2005 (sigh)
      end record;

   type Rec2 is
      record
         Next : access Rec2;                 -- Self references through anonymous access type
      end record;

   procedure P (F : access function return Integer) is -- Parameter of anonymous access to SP type
   begin
      null;
   end P;

   function F return access Integer is       -- Return type of anonymous access type
   begin
      return X : access Integer do           -- Extended return statement with body
         X := new Integer;
         return;
      end return;
      return X : access Integer;             -- Extended return statement without body
   end F;

   function G return access function return access procedure -- Return type of anonymous access to function type
   is
   begin
      return null;
   end G;

   generic
      X : in     access Integer;   -- Generic anonymous access in (not accepted by Gnat)
      Y :        access Integer;   -- Generic anonymous access in
      Z : in out access Integer;   -- Generic anonymous access in out
   procedure GP;
   procedure GP is begin null; end GP;

   package New_Types is
      type T is tagged;            -- incomplete tagged
      type T_Access is access all T;
      type T is tagged null record;
   end New_Types;

   procedure Null_Proc is null;
begin
   X := F;
   Tab(1) := X;
end Tfw_2005;
