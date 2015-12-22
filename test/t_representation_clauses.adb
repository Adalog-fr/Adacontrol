with System.Storage_Elements; use System, System.Storage_Elements;
with Ada.Streams;
with X_Representation_Clauses;
procedure T_representation_clauses is

   type T is range 1 .. 10;
   for T'Size use 8;                 -- 'Size

   X,Y : T;
   for X'Address use To_Address (0); -- 'Address
   for Y use at To_Address (4);      -- 83 address clause
   for Y'Size use 16;                -- object 'Size, 'Size

   I, J, K: Integer;
   for I'Address use J'Address;      -- 'Address, rng_over, overlay

   C1      : constant System.Address := I'Address;
   C2      : constant System.Address := T_Representation_Clauses.C1;
   for K use at C2;                  -- 83 address clause, rng_over, overlay

   type Enum is (A, B, C);
   for Enum use (10, 20, 30);        -- Enumeration
   for Enum'Alignment use 4;         -- 'Alignment

   type Enum2 is new Enum;
   for Enum2 use (4, 5, 6);          -- Der_Enum, Enumeration
   for Enum2'Alignment use 1;        -- Der_Align, 'Alignment

   type Rec is
      record
         X : Integer;
         C : Character;
      end record;
   for Rec use                       -- record, 83 alignment, bit_order
      record at mod 4;
         X at 0 range 0..31;
         C at 4 range 0..7;
      end record;

   type Der_Rec is new Rec;
   for Der_Rec use                  -- der_record, record
      record
         X at 0 range 0..31;
         C at 4 range 0..7;
      end record;
   for Der_Rec'Bit_Order use Low_Order_First;

   type Tagged_Type is tagged
      record
         X : Integer;
      end record;
   for Tagged_Type use              -- Tag_record, record, bit_order
      record
         X at 9 range 0 .. 31;      -- gap (may depend on compiler)
      end record;

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
   begin return (X => 1); end;
   procedure My_Output(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type) is
   begin null; end;

   type Tab is array (1 .. 10) of Integer;
   for Tab'Size use Integer'Size * 10;   -- arr_size, 'Size, non_power2
   V1 : Tab;
   V2 : Tab;
   for V2'Address use V1'Address;        -- Arr_Addr, 'Address, tab_over, overlay
   type RecTab is
      record
         S : String (1 .. 10);
      end record;
   for RecTab'Bit_Order use Low_Order_First;
   for RecTab'Size use 88;               -- rec_size, 'size, non_power2
   for RecTab use                        -- layout
     record
         S at 0 range 1 .. 80;           -- gap x2, arr_comp, unaligned
      end record;

   procedure Fractional_Size          is separate;
   procedure Non_Contiguous_Unaligned is separate;
   procedure Incomplete_Record        is separate;
begin
   null;
end T_representation_clauses;
