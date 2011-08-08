separate (t_movable_accept_statements)
task body Separate_Task is
begin
   accept Out_Parameter (X : out Boolean) do
      X := False;                                    -- should not trigger
   end Out_Parameter;
end Separate_Task;
