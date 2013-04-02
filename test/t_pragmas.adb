with Ada.Text_Io;

pragma Elaborate_All (Ada.Text_Io);--## rule line off pragmas

pragma Elaborate_All (Ada.Text_Io);

--## rule off pragmas
pragma Elaborate_All (Ada.Text_Io);
--## rule on pragmas

pragma Elaborate_All (Ada.Text_Io);

pragma Inconnu;
pragma TODO;

procedure T_pragmas is
   pragma Inline (T_pragmas);
   pragma Annotate (Test_Case_105); -- A GNAT pragma

   procedure P;
   procedure P (X : Integer);
   pragma Convention (C, P);                          -- multiple
   procedure P is begin null; end P;
   procedure P (X : Integer) is begin null; end P;

   function "+" (L : Integer)    return Integer;
   function "+" (L, R : Integer) return Integer;
   pragma Convention (C, "+");                        -- multiple
   function "+" (L : Integer)  return Integer is begin return 1; end "+";
   function "+" (L,R : Integer)return Integer is begin return 1; end "+";
begin
   null;
end T_pragmas;
