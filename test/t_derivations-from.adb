separate (T_derivations)
procedure From is
   -- Check plain types
   type T1 is new Integer;                -- From: Standard.Integer
   type T2 is new T1 range 1 .. 10;       -- From: Standard.Integer

   type T3 is tagged
      record
         F : Float;
      end record;
   type T4 is new T3 with null record;    -- From: T3
   type T5 is new T4                      -- From: T4
     with record
      I : Integer;
   end record;

   type T6 is range 1 .. 10;
   type T7 is new T6;                     -- From: category range
   type T8 is (One, Two, Three);
   type T9 is new T8;                     -- From: category ()

   type T10 is new Float;                 -- From: package Standard

   generic
      type F1 is new Integer;             -- From: Standard.Integer
      type F2 is new T2;                  -- From: Standard.Integer
      type F3 is new T5 with private;     -- From: T4
   procedure P;
   procedure P is
      type DF1 is new F1;                 -- From: Standard.Integer
      type DF2 is new F2;                 -- From: Standard.Integer
   begin null; end P;

   -- Check subtypes
   subtype S1 is Integer range 1 .. 10;
   type S2 is new S1;                     -- From: Standard.Integer
   subtype S3 is S2 range 1 .. 5;
   type S4 is new S3;                     -- From: S3
   type S5 is new S3'Base;                -- From: Standard.Integer;
   type S6 is new S4;                     -- From: S3

begin
   null;
end From;
