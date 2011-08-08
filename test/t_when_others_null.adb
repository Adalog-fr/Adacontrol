procedure T_when_others_null is

   type Atype is (a,b,c,d,e);
   type Btype is range 1 .. 10;

   VA : Atype := a;
   VB : Btype := 1;

   ----------------------------------Test Cases--------------------------------
   -- Nesting (N = no nesting, C = nested with case, E = nested with exception)
   -- with/without means with/without a "when others => null;"
   ----------------------------------------------------------------------------
   -- Type    N with     N without  C with     C without  E with     E without
   ----------------------------------------------------------------------------
   -- Case     1          3          5          7          9          11
   --
   -- Exce     2          4          6          8          10         12
   ----------------------------------------------------------------------------

   function X return Atype is
   begin
      return B;
   exception
      -- Test case 2
      when others =>
         null;
   end X;

   function Y return Atype is
   begin
      return B;
   exception
      when others =>
         case VB is
            when 1 =>
               null;
            when 2 =>
               begin
                  VB := VB+1;
               exception
                  -- Test case 10
                  when others =>
                     null;
               end;
            when 3 =>
               null;
            when others =>
               -- Test case 8
               VB := VB-1;
               null;
         end case;
   end Y;

begin

   case VA is
      when A =>
         case VB is
            when 1 =>
               null;
            when 2 =>
               null;
            when 3 =>
               null;
            when others =>
               -- Test case 7
               case X is
                  when A =>
                     null;
                  when B =>
                     null;
                  when C =>
                     null;
                  when others =>
                     -- Test case 5
                     null;
               end case;
         end case;
      when B =>
         begin
            VB := VB+1;
         exception
            when others =>
               case X is
                  when A =>
                     null;
                  when B =>
                     null;
                  when C =>
                     null;
                  when others =>
                     -- Test case 6
                     null;
               end case;
         end;
      when C =>
         null;
      when others =>
         -- Test case 1
         null;
   end case;
   case VB is
      when 1 =>
         null;
      when 2 =>
         begin
            VB := VB+1;
         exception
            when Constraint_Error =>
               null;

            -- Test case 9
            when others =>
               null;
               null;
         end;
      when 3 =>
         null;
      when others =>
         -- Test case 3
         VB := VB+1;
         null;
   end case;
   case X is
      when A =>
         null;
      when B =>
         begin
            VB := VB+1;
         exception
            -- Test case 11
            when others =>
               null;
               VB := VB-1;
         end;
      when C =>
         null;
      when others =>
         -- Test case 3
         VB := VB-1;
   end case;

exception
   when Constraint_Error =>
      begin
         VB := VB+1;
      exception
         when others =>
            -- Test case 12
            VB := VB-1;
            null;
      end;

   when others =>
      -- Test case 4
      null;
      VB := VB+1;

end T_when_others_null;
