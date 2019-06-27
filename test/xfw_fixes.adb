pragma Ada_2012;
package body Xfw_Fixes is

   ---------
   -- Foo --
   ---------

   procedure Foo is
      procedure Bar is
      begin
         null;
      end Bar;
   begin
      Bar;
   end Foo;

end Xfw_Fixes;
