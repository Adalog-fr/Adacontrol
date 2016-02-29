separate (t_statements.test_raise)
package body Pack1 is
begin
   raise Constraint_Error;  -- raise, raise_standard
   raise False_External;    -- raise
end Pack1;
