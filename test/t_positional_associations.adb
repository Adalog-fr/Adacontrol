procedure T_Positional_Associations is
   type Arecord is
      record
         A : Integer;
         B : Integer;
         C : Integer;
      end record;
   type Trecord is tagged
      record
         D : Integer;
      end record;
   type ETrecord is new Trecord with
      record
         E : Integer;
      end record;
   type NTrecord is new Trecord with null record;


   function Nothing ( X : in Integer ) return Integer is
   begin
      return X;
   end Nothing;
   procedure Nproc ( Y : in Integer ; Z : in Integer) is
   begin
      null;
   end Nproc;
   procedure Nproc ( Y : in Integer ; Z : in Integer; T : in Float) is
   begin
      null;
   end Nproc;
   Variable : Integer;
   RecordI : Arecord;
   RecordIT : Trecord;
   RecordITE : ETrecord;
   Max : constant := 5;
   Tab : array (1 .. Max) of Integer;

   generic
      Elem1 : in Integer;
      Elem2 : in Integer := 0;
   procedure Pgen;

   procedure Pgen is
   begin
      null;
   end Pgen;

   generic
      Elem1 : in Integer;
      Elem2 : in Integer := 0;
   function Fgen return Integer;

   function Fgen return Integer is
   begin
      return Elem1+Elem2;
   end Fgen;

   generic
      Elem1 : in Integer;
      Elem2 : in Integer := 0;
   package Pac_Gen is
   end Pac_Gen;

   package body Pac_Gen is
   begin
      null;
   end Pac_Gen;


   procedure Ppgen1 is new Pgen (Elem1 => 1) ;
   procedure Ppgen2 is new Pgen (Elem1 => 1, Elem2 => 1) ;
   procedure Ppgen3 is new Pgen (1, 1);               -- All x2, Declared x2
   procedure Ppgen4 is new Pgen (1);                  -- Declared

   function Ffgen1 is new Fgen (Elem1 => 1);
   function Ffgen2 is new Fgen (Elem1 => 1, Elem2 => 1);
   function Ffgen3 is new Fgen (1, 1);                -- All x2, Declare x2
   function Ffgen4 is new Fgen (1);                   -- Declared

   package Ppac_Gen1 is new Pac_Gen (Elem1 => 1);
   package Ppac_Gen2 is new Pac_Gen (Elem1 => 1, Elem2 => 1);
   package Ppac_Gen3 is new Pac_Gen (1, 1);           -- All x2, Declared x2
   package Ppac_Gen4 is new Pac_Gen (1);              -- Declared

   task Taske is
      entry EntryCall ( I : in Integer; J : in Integer := 0);
   end Taske;
   task body Taske is
   begin
      accept EntryCall ( I : Integer; J : in Integer := 0) do
         null; end EntryCall;
      accept EntryCall (I : Integer; J : in Integer := 0);    -- OK
   end Taske;

   type Enum1 is (X, Y);
   for Enum1 use (X => 1, Y => 2);                            -- OK

   type Enum2 is (A, B);
   for Enum2 use (1, 2);                                     -- all_positional x2

   package Object is
      type Instance is tagged null record;
      procedure Method (Self : in out Instance; P1 : Integer := 1);
   end Object;
   package body Object is
      procedure Method (Self : in out Instance; P1 : Integer := 1) is
      begin
         null;
      end Method;
   end Object;
   use Object;
   Obj : Object.Instance;

begin
   Variable := Nothing (X => 1);
   Variable := Nothing (1);                           -- OK, count 1
   Nproc ( Y => Variable, Z => 1);
   Nproc (0, 1);                                      -- All x2, Same_Type x2, All_Positional x2, Declared x2, count 2
   Nproc (0, Z => 1);                                 -- All, declared, count 1
   Nproc (0, 1, 1.0);                                 -- OK for All (exception to the rule), All_Positional x3, Same_Type x2, Declared x3, count 3
   RecordI := (A => 1, B => 0, C => 1);
   RecordI := (1, 0, 1);                              -- All x3
   RecordIT := Trecord'(D => 1);
   RecordITE := (RecordIT with E => 1);
   RecordITE := (RecordIT with 1);                    -- OK
   Tab := (1, 0, 1, 0, 1);                            -- All x5
   Tab := (1 => 0, 2 => 1, 3 => 0, 4 => 1, 5 => 0);
   Taske.EntryCall (1);                               -- Declared, count 1
   Taske.EntryCall (I => 1);                          -- OK
   Taske.EntryCall (1, 2);                            -- All x2, All_Positional x2, Same_Type x2, Declared x2, count 2
   Taske.EntryCall (I => 1, J => 1);                  -- OK

   Tab (1) := Tab (1) + 1;                            -- OK
   Tab (1) := "+" (Tab (1), 1);                       -- OK for all (because not_operator), All_Positional x2, Same_Type x2, count 2

   Method (Obj, 1);                                   -- All x2, All_Positional x2, Declared x2, count 2
   Obj.Method (1);                                    -- OK (prefix op not counted), count 1
end T_Positional_Associations;
