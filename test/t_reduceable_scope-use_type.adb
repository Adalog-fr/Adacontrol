separate (T_Reduceable_Scope)
procedure Use_Type is
   package Pack is
      type Enum is (A, B, C, 'X');
      type Int is range 1 .. 10;
   end Pack;

   package Inner is                -- Movable to block
      type Der_Int is new Pack.Int;
      type Der_Enum is new Pack.Enum;
   end Inner;

   package To_Body is              -- Not used
      use type Pack.Int;           -- Movable to Proc
      use all type Pack.Enum;      -- Movable to body
      I : Pack.Int;                -- Movable to body
      E : Pack.Enum;               -- Movable to body
   end To_Body;

   package body To_Body Is
      procedure Proc is            -- Not used
      begin
         I := I + 1;
      end Proc;
   begin
      E := 'X';
   end To_Body;
begin
   null;

   declare
      use type Pack.Int;           -- Movable to block
      use all type Pack.Enum;      -- Movable to block
      I : Pack.Int;                -- Movable to block
      E : Pack.Enum;               -- Movable to block
   begin
      begin
         I := I + 1;
         E := A;
      end;
   end;

   declare
      use type Pack.Int;           -- OK
      use all type Pack.Enum;      -- OK
      I : Pack.Int;
      E : Pack.Enum;
   begin
      I := I - 1;
      E := B;

      begin
         I := I + 1;
         E := A;
      end;
   end;

   declare
      use type Inner.Der_Int;      -- Movable to block
      use all type Inner.Der_Enum; -- Movable to block
      I : Inner.Der_Int;           -- Movable to block
      E : Inner.Der_Enum;          -- Movable to block
   begin
      begin
         I := I + 1;
         E := A;
      end;
   end;

end Use_Type;
