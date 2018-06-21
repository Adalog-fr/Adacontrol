separate (T_Uncheckable)
procedure Allocators is
   type Rec (D : access Integer) is
      record
         F : access Integer;
      end record;
   type Tab is array (1 .. 10) of access Integer;
   VR : Rec (new Integer);        -- Uncheckable, Usage
   VT : Tab;                      -- Usage
begin
   VR := (VR.D, new Integer);     -- Uncheckable
   VT := (others => new Integer); -- Uncheckable
end Allocators;
