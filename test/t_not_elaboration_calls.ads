with Ada.Text_IO; use Ada.Text_IO;
package T_not_elaboration_calls is
   procedure P;

   F : File_Type;
   X : Count := Line (F);
end T_not_elaboration_calls;
