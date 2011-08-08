procedure T_Directly_Accessed_Globals is
   package P1 is
      function Get_Next return Integer;
   end P1;
   package body P1 is    -- OK
      I1 : Integer;
      Count : Integer := 0;
      S1 : String (1..10);
      S2 : String renames S1;
      S3 : String (1..10);

      procedure Read is
         I : Integer;
      begin
         I := I1;
         if S1 = "" then
            null;
         end if;
         if S2 = "" then
            null;
         end if;
      end Read;

      procedure Write is
      begin
         I1 := 1;
         S1 (1) := 'a';
         S1 := (others => ' ');
         S2 := (others => ' ');
      end Write;

      procedure Update is
      begin
         S3 (1) := 'a';
         if S3 = "" then
            null;
         end if;
      end Update;

      function Get_Next return Integer is
      begin
         Count := Count + 1;
         return Count;
      end Get_Next;

      package Pack1 is
         protected Prot is
            function  Read return Integer;
            entry     Write;
            procedure Read_Write;
         end Prot;
      end Pack1;

      package body Pack1 is
         I1, I2 : Integer;

         protected body Prot is
            function Read return Integer is
            begin
               return I1;
            end Read;

            entry Write when True is
            begin
               I2 := 1;
            end Write;

            procedure Read_Write is
            begin
               I1 := I2;
            end Read_Write;
         end Prot;
      end Pack1;

      package Pack2 is
         task T is
            entry Read;
            entry Write;
            entry Read_Write;
         end T;
      end Pack2;

      package body Pack2 is
         I1, I2 : Integer;

         task body T is
            Local : Integer;
         begin
            accept Read do
               Local := I1;
            end Read;

            accept Write do
               I2 := 1;
            end Write;

            accept Read_Write do
               I1 := I2;
            end Read_Write;
         end T;
      end Pack2;
   end P1;

   package P2 is
   end P2;
   package body P2 is
      I1 : Integer;
      Count : Integer := 0;
      S1    : String (1 .. 10);
      S2    : Character renames S1 (I1);         -- Not from subprogram I1, OK S1

      type Rec is
         record
            I, J : Integer;
         end record;
      R : Rec;

      package Pack is
         G1 : aliased Integer;                -- OK, not package body
      end Pack;

      package body Pack is
         G2, G3, G4 : Integer;                -- G2 not read, G3 not written, G4 not read/written
         procedure P1 is
         begin
            G2 := 1;
         end P1;
         procedure P2 is
         begin
            G1 := G3;
         end P2;
      end Pack;

      procedure Read1 is
         I : Integer;
      begin
         I := I1;
         if S1 = "" then
            null;
         end if;
         if S2 = 'a' then
            null;
         end if;
         if R.I = 1 then
            null;
         end if;
      end Read1;

      procedure Write is
      begin
         I1 := 1;
         S1 (1) := 'a';
         S1 := (others => ' ');
         R.J := 0;
      end Write;

      procedure Update is
         procedure Inner is
         begin
            I1 := 1;                -- Nested subprogram, Written
         end Inner;
         Ren1 : Integer renames R.I;
         Ren2 : Integer renames R.J;
      begin
         I1 := 1;                   -- already written
         S2 := 'a';                 -- already written
         if S1 = "" then            -- already read
            null;
         end if;
         R.J  := R.I;               -- already written, already read
         Ren1 := Ren2;              -- already written, already read
      end Update;

      function Get_Next return Integer is
      begin
         Count := Count + 1;
         return Count;
      end Get_Next;

      function Get_Current return Integer is
      begin
         return Count;                 -- already read
      end Get_Current;

      generic
      procedure Gen;

      procedure Gen is
      begin
         I1 := 1;                      -- Generic subprogram, already written
      end Gen;
   begin
      S2 := 'a';                       -- Not from subprogram
   end P2;

   package P3 is
   end P3;
   package body P3 is
      I1 : Integer;
      I2 : Integer;                    -- Not from same protected object
      I3 : Integer;

      protected type Prot1 is
         function  Read return Integer;
         entry     Write;
      end Prot1;

      protected body Prot1 is
         function Read return Integer is
         begin
            return I1;             -- From protected type
         end Read;

         entry Write when True is
         begin
            I1 := 1;              -- From protected type
         end Write;
      end Prot1;

      protected Prot21 is
         function  Read return Integer;
      end Prot21;

      protected body Prot21 is
         function Read return Integer is
         begin
            return I2;
         end Read;
      end Prot21;

      protected Prot22 is
         entry Write;
      end Prot22;

      protected body Prot22 is
         entry Write when True is
         begin
            I2 := 1;
         end Write;
      end Prot22;

      procedure Proc is
         protected Prot3 is
            function  Read return Integer;
            entry     Write;
         end Prot3;

         protected body Prot3 is
            function Read return Integer is
            begin
               return I3;             -- Nested PO
            end Read;

            entry Write when True is
            begin
               I3 := 3;              -- Nested PO
            end Write;
         end Prot3;
      begin
         null;
      end Proc;
   end P3;

   package P4 is
   end P4;
   package body P4 is
      I1 : Integer;
      I2 : Integer;                    -- Not from same task object
      I3 : Integer;

      task type Task1 is
         entry Read;
         entry Write;
      end Task1;

      task body Task1 is
         Local : Integer;
      begin
         accept Read do
            Local := I1;             -- From task type
         end Read;

         accept Write do
            I1 := 1;                 -- From task type
         end Write;
      end Task1;

      task Task21 is
         entry  Read;
      end Task21;

      task body Task21 is
         Local : Integer;
      begin
         accept Read do
            Local := I2;
         end Read;
      end Task21;

      task Task22 is
         entry Write;
      end Task22;

      task body Task22 is
      begin
         accept Write do
            I2 := 1;
         end Write;
      end Task22;

      procedure Proc is
         task Task3 is
            entry Read;
            entry Write;
         end Task3;

         task body Task3 is
            Local : Integer;
         begin
            accept Read do
               Local := I3;             -- Nested task
            end Read;

            accept Write do
               I3 := 3;                 -- Nested task
            end Write;
         end Task3;
      begin
         null;
      end Proc;
   end P4;

begin
   null;
end T_Directly_Accessed_Globals;
