pragma Ada_2005;
procedure Tfw_2005 is
   -- Only for stress test.
   -- Does not test anything by itself (hence considered tfw),
   -- but contains a little digest of the most error-prone functionalities
   -- of Ada 2005, to make sure new rules are not trapped by them.
   
   X : access Integer;
   Tab : array (1..10) of access Integer; 
   type Rec is
      record
	 F : access Integer'Base'Base;
	 -- Several 'Base were already allowed before 2005 (sigh)
      end record;

   procedure P (F : access function return Integer) is
   begin
      null;
   end P;
   
   function F return access Integer is
   begin
      return X : access Integer do
         X := new Integer;
         return;
      end return;
   end F;
   
   --  For the moment, this little gem crashes Gnat...
   --  function G return access function return access procedure 
   --  is 
   --  begin
   --     return null;
   --  end G;
begin
   X := F;
   Tab(1) := X;
end Tfw_2005;
