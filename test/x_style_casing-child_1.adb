package body X_Style_Casing.CHILD_1 is    -- casing_identifier

   procedure P1 is
   begin
      BLock1 : declare
      begin
         null;
      end Block1;

      LoOp1 : for I in 1 .. 10 loop
         null;
      end loop Loop1;
   end P1;

   task TT is
      entry Enter;
   end Tt;

   task body Tt is                        -- casing_identifier
   begin
      accept ENTER do                     -- casing_identifier
         null;
      end ENTER;
   end Tt;

   protected type PT is
      entry Ent;
   end Pt;

   protected body Pt is                   -- casing_identifier
      entry ENT when True is              -- casing_identifier
      begin
         null;
      end ENT;
   end Pt;
end X_STYLE_CASING.Child_1;
