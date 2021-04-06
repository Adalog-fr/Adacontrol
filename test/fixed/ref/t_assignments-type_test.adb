with Ada.Streams;
separate (T_Assignments)
procedure Type_Test is
   type T  is range 1 .. 10;
   type D1 is new T;
   type D2 is new T;
   type D3 is new T'Base;

   type Tagged_T  is tagged null record;
   type Tagged_D1 is new Tagged_T with null record;
   type Tagged_D2 is new Tagged_D1 with null record;

   type Rec_T is
      record
         I : Integer;
         C : Character;
         W : Wide_Character;
      end record;
   type Rec_D1 is new Rec_T;

begin
   -- Simple type
   declare
      VT  : T;
      VD1 : D1;
      VD2 : D2;
      VD3 : D3;
   begin
      VT  := 1;    -- ancestor T
      VD1 := 1;    -- Direct D1, ancestor T
      VD2 := 1;    -- ancestor T
      VD3 := 1;    -- ancestor T
   end;

   -- tagged type
   Try_Tagged:
   declare
      VT  : Tagged_T;
      VD1 : Tagged_D1;
      VD2 : Tagged_D2;
      VD3 : Tagged_D1;
   begin
      VT  := @;             -- ancestor Tagged_T, Trivial_Target_Name
      VD1 := @;            -- directe Tagged_D1, ancestor Tagged_T, Trivial_Target_Name
      Try_Tagged.VD2 := VD2; -- ancestor Tagged_T
      Tagged_T (VD3) := VT;  -- ancestor Tagged_T
   end Try_Tagged;

   -- Array component
   declare
      use Ada.Streams;
      type New_Stream is new Stream_Element_Array;
      S1, S2 : New_Stream (1 .. 10);
   begin
      S1 (1) := 0;             -- component Stream_Element_Array
      S2     := (others => 1); -- whole stream
   end;

   -- Record component
   declare
      VT  :  Rec_T;
      VD1 : Rec_D1;
   begin
      VT.I  := 1;  -- component_ances Rec_T
      VD1.I := 2;  -- component Rec_D1, component_ances Rec_T;
   end;
end type_test;
