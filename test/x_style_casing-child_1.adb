package body X_Style_Casing.CHILD_1 is    -- casing_identifier

   procedure P1 is
   begin
      BLock1 : declare
      begin
         null;
      end BlOck1;                         -- casing_identifier

      LoOp1 : for I in 1 .. 10 loop
         null;
      end loop LOop1;                     -- casing_identifier
   end P1;

   task TTT is
      entry Enter;
   end TTt;                               -- casing_identifier

   task body TtT is                       -- casing_identifier
   begin
      accept ENTER do                     -- casing_identifier
         null;
      end ENTER;                          -- casing_identifier
   end TTt;                               -- casing_identifier

   protected type PTP is
      entry Ent;
   end PtP;                               -- casing_identifier

   protected body PtP is                  -- casing_identifier
      entry ENT when True is              -- casing_identifier
      begin
         null;
      end ENT;                            -- casing_identifier
   end PTp;                               -- casing_identifier
end X_STYLE_CASING.Child_1;               -- casing_identifier
