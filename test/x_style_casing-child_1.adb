package body X_Style_Casing.CHILD_1 is    -- casing_identifier

   procedure P1 is
   begin
      BLock1 : declare
      begin
         null;
      end Block1;                         -- casing_identifier

      LoOp1 : for I in 1 .. 10 loop
         null;
      end loop Loop1;                     -- casing_identifier
   end P1;

   task TT is
      entry Enter;
   end Tt;                                -- casing_identifier

   task body Tt is                        -- casing_identifier
   begin
      accept ENTER do                     -- casing_identifier
         null;
      end ENTER;                          -- casing_identifier
   end Tt;                                -- casing_identifier

   protected type PT is
      entry Ent;
   end Pt;                                -- casing_identifier

   protected body Pt is                   -- casing_identifier
      entry ENT when True is              -- casing_identifier
      begin
         null;
      end ENT;                            -- casing_identifier
   end Pt;                                -- casing_identifier
end X_STYLE_CASING.Child_1;               -- casing_identifier
