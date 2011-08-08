separate (T_Declarations)
procedure Predefined_Operator is         -- nested procedure, local_procedure
   type Int is new Integer;              -- derived_type
   type Bool is new Boolean;             -- derived type
   type ArrB is array (1 .. 10) of Bool; -- constrained_array_type, array
   type ArrBB is array (1..3) of ArrB;   -- constrained_array_type, array

   function "+" (L:Integer; R : Integer) return Integer is             -- operator, predefined_operator
   begin return 1; end;
   function "-" (L:Integer; R : Integer'base) return Integer'base is   -- operator, predefined_operator
   begin return 1; end;
   function "+" (L : Int; R : Integer) return Integer is               -- operator
   begin return 1; end;

   function "*" (L : Duration; R : Integer) return Duration is  -- operator, predefined_operator
   begin return 1.0; end;
   function "*" (L : Duration; R : Int) return Duration is      -- operator
   begin return 1.0; end;
   function "*" (L : Duration; R : Integer) return Integer is   -- operator
   begin return 1; end;

   function "**" (L : Float; R : Integer) return Float is       -- operator, predefined operator
   begin return 1.0; end;
   function "**" (L : Float; R : Int) return Float is           -- operator
   begin return 1.0; end;

   function "+" (L : ArrB; R : ArrB) return ArrB is             -- operator
   begin return (others => False); end;

   function "and" (L : ArrB; R : ArrB) return ArrB is          -- operator, predefined_operator
   begin return (others => False); end;

   function "&" (L : ArrBB; R : ArrBB) return ArrBB is         -- operator, predefined_operator
   begin return (others => (others =>False)); end;

   function "&" (L : ArrB; R : ArrB) return ArrB is            -- operator, predefined_operator
   begin return (others => False); end;

   function "+" (L : ArrB) return ArrB is                      -- operator
   begin return (others => False); end;

   function "not" (L : ArrB) return ArrB is                    -- operator, predefined_operator
   begin return (others => False); end;

begin
   null;                                                       -- null procedure
end Predefined_Operator;
