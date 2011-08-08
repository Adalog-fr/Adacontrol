with System.Storage_Elements; use System.Storage_Elements;
with Ada.Streams;
procedure T_representation_clauses is
   type T is range 1 .. 10;
   for T'Size use 8;

   X,Y : T;
   for X'Address use To_Address (0);
   for Y use at To_Address (4);

   type Enum is (A, B, C);
   for Enum use (10, 20, 30);
   for Enum'Alignment use 4;

   type Rec is
      record
         X : Integer;
         C : Character;
      end record;
   for Rec use
      record at mod 4;
         X at 0 range 0..31;
         C at 4 range 0..7;
      end record;

   type Tagged_Type is tagged null record;
   function My_Input(Stream : access Ada.Streams.Root_Stream_Type'Class) return Tagged_Type;
   procedure My_Output(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type);
   procedure My_Read(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Tagged_Type'Class);
   procedure My_Write(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type'Class);

   for Tagged_Type'Input use My_Input;
   for Tagged_Type'Output use My_Output;
   for Tagged_Type'Class'Read use My_Read;
   for Tagged_Type'Class'Write use My_Write;

   procedure My_Read(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Tagged_Type'Class) is
   begin null; end;
   procedure My_Write(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type'Class) is
   begin null; end My_Write;
   function My_Input(Stream : access Ada.Streams.Root_Stream_Type'Class) return Tagged_Type is
   begin return (null record); end;
   procedure My_Output(Stream : access Ada.Streams.Root_Stream_Type'Class; Item : in Tagged_Type) is
   begin null; end;

begin
   null;
end T_representation_clauses;
