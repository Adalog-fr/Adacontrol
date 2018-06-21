procedure T_Return_Type.Anonymous_Access is
   function Anonymous_Access_1 return access Integer is     -- Anonymous_access
   begin
      return new Integer'(1);
   end Anonymous_Access_1;

   function Anonymous_Access_2 return access procedure is   -- Anonymous_access
   begin
      return Anonymous_Access'Access;
   end Anonymous_Access_2;

begin
   null;
end T_Return_Type.Anonymous_Access;
