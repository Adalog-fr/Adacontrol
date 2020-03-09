procedure T_Simplifiable_Statements is
   procedure Test_Block                         is separate;
   procedure Test_Dead                          is separate;
   procedure Test_For_For_Slice                 is separate;
   procedure Test_For_In_For_For_Of             is separate;
   procedure Test_Handler                       is separate;
   function  Test_If return Integer             is separate;
   procedure Test_If_For_Case                   is separate;
   procedure Test_If_Not                        is separate;
   procedure Test_Loop                          is separate;
   procedure Test_Null                          is separate;
   procedure Test_True_If                       is separate;
   function  Test_Unnecessary_If return Boolean is separate;
   procedure Test_While_For_For                 is separate;
begin
   null;
end T_Simplifiable_Statements;
