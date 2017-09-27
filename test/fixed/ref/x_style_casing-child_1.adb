package body X_Style_Casing.Child_1 is    -- casing_identifier

   procedure P1 is
   begin
      BLock1 : declare
      begin
         null;
      end BLock1;                         -- casing_identifier

      LoOp1 : for I in 1 .. 10 loop
         null;
      end loop LoOp1;                     -- casing_identifier
   end P1;

   task TTT is
      entry Enter;
   end TTT;                               -- casing_identifier

   task body TTT is                       -- casing_identifier
   begin
      accept Enter do                     -- casing_identifier
         null;
      end Enter;                          -- casing_identifier
   end TTT;                               -- casing_identifier

   protected type PTP is
      entry Ent;
   end PTP;                               -- casing_identifier

   protected body PTP is                  -- casing_identifier
      entry Ent when True is              -- casing_identifier
      begin
         null;
      end Ent;                            -- casing_identifier
   end PTP;                               -- casing_identifier
end X_Style_Casing.Child_1;               -- casing_identifier
