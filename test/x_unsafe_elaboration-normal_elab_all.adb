with X_Unsafe_Elaboration.Normal4;  -- no Elaborate
package body X_Unsafe_Elaboration.Normal_Elab_All is
   package body Gen is
      function FG return Integer is
      begin
         X_Unsafe_Elaboration.Normal4.Proc;
         return 0;
      end FG;
   end Gen;
end X_Unsafe_Elaboration.Normal_Elab_All;
