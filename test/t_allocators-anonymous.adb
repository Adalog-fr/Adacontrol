separate (T_Allocators)
procedure Anonymous is
   V : access Integer;
   type Rec is
      record
	 I: Integer;
	 F: access Integer;
      end record;
   VR : Rec;
   procedure P (X : access Integer) is
   begin
      null;
   end;
   generic
      GV : access Integer;
   procedure GP;
   procedure GP is begin null; end GP;
   procedure Inst is new Gp (new Integer);       -- Anonymous

   Tab : array (1..10) of access Integer;

begin
   V := new Integer;                             -- Anonymous
   VR.F := new Integer;                          -- Anonymous
   VR := (I => 0, F => new Integer);             -- Anonymous
   VR := (0, new Integer);                       -- Anonymous
   P (new Integer);                              -- Anonymous
   Tab (1) := new Integer;                       -- Anonymous
end Anonymous;
