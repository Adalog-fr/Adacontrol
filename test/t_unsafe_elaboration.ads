with X_Unsafe_Elaboration.Normal_Elab_All;
pragma Elaborate_All (X_Unsafe_Elaboration.Normal_Elab_All);  -- Ticket #38
package T_Unsafe_Elaboration is
   procedure Exported1;
   procedure Exported2;
   package Inst1 is new X_Unsafe_Elaboration.Normal_Elab_All.Gen;
end T_Unsafe_Elaboration;
