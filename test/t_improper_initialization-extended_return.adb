pragma Ada_2005;
separate (T_Improper_Initialization)
function Extended_Return return Integer is
   A : Integer := 3;         -- unnecessarily initialized
   B : Integer;              -- not safely initialized
begin
   A := 1;
   case A is
      when 1 =>
         return X : Integer := 3 do   -- OK
            return;
         end return;
      when 2 =>
         return X : Integer do        -- not safely initialized
            return;
         end return;
      when 3 =>
         declare
            C : Integer;               -- used before initialization
         begin
            return X : Integer := C do -- use of uninitialized C, unnecessarily initialized
               X := 1;
               return;
            end return;
         end;
      when others =>
         declare
            D : Integer;              -- not safely initialized
         begin
            if A = 0 then
               D := 1;
            else
               return X : Integer do  -- not safely initialized
                  goto Hell;
               end return;
            end if;
            <<Hell>> null;
         end;
   end case;
end Extended_Return;
