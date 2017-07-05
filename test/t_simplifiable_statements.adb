procedure T_Simplifiable_Statements is
   procedure Test_Block             is separate;
   procedure Test_Handler           is separate;
   function  Test_If return Integer is separate;
   procedure Test_If_For_Case       is separate;
   procedure Test_If_Not            is separate;
   procedure Test_Loop              is separate;
   procedure Test_Null              is separate;
   procedure Test_Dead              is separate;
begin
   null;
end T_Simplifiable_Statements;
