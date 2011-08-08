separate (T_Expressions)
procedure Test_Attributes is
   subtype Index is Integer range 1 .. 10;

   -- One dimensional:
   type T11 is array (Index) of Integer;
   type T12 is array (Positive range <>) of Integer;
   type T13 is new T12 (Index);
   subtype T14 is T12 (Index);

   V11 : T11;
   V12 : T12 (Index);
   V13 : T13;
   V14 : T14;
   V15 : array (Index) of Integer;

   -- Two dimensional:
   type T21 is array (Index, Index) of Integer;
   type T22 is array (Positive range <>, Character range <>) of Integer;
   type T23 is new T22 (Index, 'a' .. 'z');
   subtype T24 is T22 (Index, 'a' .. 'z');

   V21 : T21;
   V22 : T22 (Index, 'a' .. 'z');
   V23 : T23;
   V24 : T24;
   V25 : array (Index, Index) of Integer;

   I : Integer;
begin
   -- Variables, 1 dimension
   I := V11'First;
   I := V11'First (1);            -- Inconsistent_Attribute_Dimension
   I := V12'Last;
   I := V12'Last (1);             -- Inconsistent_Attribute_Dimension
   for I in V13'Range loop
      null;
   end loop;
   for I in V13'Range (1) loop    -- Inconsistent_Attribute_Dimension
      null;
   end loop;
   I := V14'Length;
   I := V14'Length (1);           -- Inconsistent_Attribute_Dimension
   I := V15'First;
   I := V15'First (1);            -- Inconsistent_Attribute_Dimension

   -- Types, 1 dimension
   I := T11'First;
   I := T11'First (1);            -- Inconsistent_Attribute_Dimension
   for I in T13'Range loop
      null;
   end loop;
   for I in T13'Range (1) loop    -- Inconsistent_Attribute_Dimension
      null;
   end loop;
   I := T14'Length;
   I := T14'Length (1);           -- Inconsistent_Attribute_Dimension

   -- Variables, 2 dimensions
   I := V21'First;                -- Inconsistent_Attribute_Dimension
   I := V21'First (2);
   I := V22'Last;                 -- Inconsistent_Attribute_Dimension
   I := V22'Last (1);
   for I in V23'Range loop        -- Inconsistent_Attribute_Dimension
      null;
   end loop;
   for I in V23'Range (2) loop
      null;
   end loop;
   I := V24'Length;               -- Inconsistent_Attribute_Dimension
   I := V24'Length (1);
   I := V25'First;                -- Inconsistent_Attribute_Dimension
   I := V25'First (2);

   -- Types, 2 dimensions
   I := T21'First;                -- Inconsistent_Attribute_Dimension
   I := T21'First (2);
   for I in T23'Range loop        -- Inconsistent_Attribute_Dimension
      null;
   end loop;
   for I in T23'Range (2) loop
      null;
   end loop;
   I := T24'Length;               -- Inconsistent_Attribute_Dimension
   I := T24'Length (1);

end Test_Attributes;
