with System.Storage_Elements; use System.Storage_Elements;
with Ada.Streams;
procedure T_representation_clauses is

   type T is range 1 .. 10;
   for T'Size use 8;                 -- 'Size

   X,Y : T;
   for X'Address use To_Address (0); -- 'Address
   for Y use at To_Address (4);      -- 83 address clause

   I, J, K: Integer;
   for I'Address use J'Address;      -- 'Address, overlay

   C1      : constant System.Address := I'Address;
   C2      : constant System.Address := T_Representation_Clauses.C1;
   for K use at C2;                  -- 83 address clause, overlay

   type Enum is (A, B, C);
   for Enum use (10, 20, 30);        -- Enumeration
   for Enum'Alignment use 4;         -- 'Alignment

   type Rec is
      record
         X : Integer;
         C : Character;
      end record;
   for Rec use                       -- Record, 83 alignment
      record at mod 4;
         X at 0 range 0..31;
         C at 4 range 0..7;
      end record;

   type Tagged_Type is tagged null record;
   function My_Input(Stream : access Ada.Streams.Root_Stream_Type'Class) return Tagged_Type;
   procedure My_Output(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type);
   procedure My_Read(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Tagged_Type'Class);
   procedure My_Write(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type'Class);

   for Tagged_Type'Input use My_Input;            -- 'Input
   for Tagged_Type'Output use My_Output;          -- 'Output
   for Tagged_Type'Class'Read use My_Read;        -- 'Class'Read
   for Tagged_Type'Class'Write use My_Write;      -- 'Class'Write

   procedure My_Read(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Tagged_Type'Class) is
   begin null; end;
   procedure My_Write(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type'Class) is
   begin null; end My_Write;
   function My_Input(Stream : access Ada.Streams.Root_Stream_Type'Class) return Tagged_Type is
   begin return (null record); end;
   procedure My_Output(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type) is
   begin null; end;

   procedure Fractional_Size       is separate;
   procedure Non_Contiguous_Record is separate;
   procedure Incomplete_Record     is separate;
begin
   null;
end T_representation_clauses;
