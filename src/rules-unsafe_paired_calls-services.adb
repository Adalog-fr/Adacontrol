----------------------------------------------------------------------
--  Rules.Unsafe_Paired_Calls.Services - Package body               --
--                                                                  --
--  This software is (c) Adalog 2004-2016.                          --
--  The Ada Controller is  free software; you can  redistribute  it --
--  and/or modify it under  terms of the GNU General Public License --
--  as published by the Free Software Foundation; either version 2, --
--  or (at your option) any later version. This unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from the units  of this program, or if you  link this unit with --
--  other files  to produce  an executable, this  unit does  not by --
--  itself cause the resulting executable  to be covered by the GNU --
--  General  Public  License.   This  exception  does  not  however --
--  invalidate any  other reasons why the executable  file might be --
--  covered by the GNU Public License.                              --
----------------------------------------------------------------------

-- ASIS
with
   Asis.Declarations,
   Asis.Elements,
   Asis.Expressions,
   Asis.Statements;

-- Adalog
with
   Thick_Queries,
   Utilities;

package body Rules.Unsafe_Paired_Calls.Services is

   Nil_Signature : constant Nesting_Signature := Nesting_Signature (Asis.Nil_Element_List);

   -------------------------
   -- Is_Boolean_Constant --
   -------------------------

   function Is_Boolean_Constant (Expr : Asis.Expression) return Boolean is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      Decl : Asis.Declaration;
   begin
      if Expression_Kind (Expr) /= An_Identifier then
         return False;
      end if;

      Decl := Corresponding_Name_Declaration (Ultimate_Name (Expr));
      if Declaration_Kind (Decl) /= A_Constant_Declaration then
         return False;
      end if;

      if To_Upper (Full_Name_Image (Subtype_Simple_Name (Object_Declaration_View (Decl)))) /= "STANDARD.BOOLEAN" then
         return False;
      end if;

      return True;
   end Is_Boolean_Constant;


   ------------------------------------------------------------------
   -- Exported subprograms
   ------------------------------------------------------------------

   ------------------------------------
   -- Effective_Last_Statement_Index --
   ------------------------------------

   function Effective_Last_Statement_Index (Stats : Asis.Statement_List) return Asis.ASIS_Natural is
      use Asis, Asis.Elements;
   begin
      for S in reverse Stats'Range loop
         case Statement_Kind (Stats (S)) is
            when A_Return_Statement
               | An_Extended_Return_Statement
               | A_Goto_Statement
               | An_Exit_Statement
               | A_Null_Statement
               =>
               null;
            when others =>
               return S;
         end case;
      end loop;

      -- Statement list contains only return, exit and null statements...
      return 0;
   end Effective_Last_Statement_Index;


   ------------------------------
   -- Effective_Last_Statement --
   ------------------------------

   function Effective_Last_Statement (Stats : Asis.Statement_List) return Asis.Statement is
      use Utilities;
      Inx : constant Asis.ASIS_Natural := Effective_Last_Statement_Index (Stats);
   begin
      if Inx = 0 then
         -- Statement list contains only return, exit and null statements...
         -- we shouldn't be here!
         Failure ("No effective statement in statements list");
      end if;

      return Stats (Inx);
   end Effective_Last_Statement;


   ---------------
   -- Signature --
   ---------------

   function Signature (Stmt : Asis.Statement; With_Check : Boolean) return Nesting_Signature is
      use Asis, Asis.Elements;

      function Enclosing_Signature (Elem : Asis.Element) return Nesting_Signature is
      -- Procedure (or entry) calls are not allowed in an Enclosing_Signature
         use Asis.Statements;

         function Nil_Or_Raise (Message : String) return Nesting_Signature is
         begin
            if With_Check then
               raise Invalid_Nesting with Message;
            else                                     --## Rule line off Simplifiable_Statements ## To keep symetry
               return Nil_Signature;
            end if;
         end Nil_Or_Raise;

      begin   -- Enclosing_Signature
         case Element_Kind (Elem) is
            when A_Statement =>
               case Statement_Kind (Elem) is
                  when An_If_Statement =>
                     if Conditionals_Allowed.Value = Off then
                        return Nil_Or_Raise ("nested calls not allowed");
                     end if;

                     declare
                        Expr : constant Asis.Expression := Condition_Expression (Statement_Paths (Elem) (1));
                     begin
                        if Is_Boolean_Constant (Expr) then
                           return Enclosing_Signature (Enclosing_Element (Elem)) & Elem & Expr;
                        else
                           return Nil_Or_Raise ("if condition is not a boolean constant");
                        end if;
                     end;
                  when others =>
                     return Nil_Signature;
               end case;

            when A_Path =>
               case Path_Kind (Elem) is
                  when An_If_Path | An_Else_Path =>
                     -- Only one effective statement allowed, must be the first statement
                     declare
                        Stats : constant Asis.Statement_List := Thick_Queries.Statements (Elem);
                     begin
                        if Effective_Last_Statement_Index (Stats) = Stats'First then
                           return Enclosing_Signature (Enclosing_Element (Elem)) & Elem;
                        else
                           return Nil_Or_Raise ("path contains disallowed statements");
                        end if;
                     end;
                  when others =>
                     return Nil_Or_Raise ("call in disallowed structured statement");
               end case;

            when others =>
               return Nil_Signature;
         end case;
      end Enclosing_Signature;

   begin   -- Signature
      if Statement_Kind (Stmt) not in A_Procedure_Call_Statement | An_Entry_Call_Statement then
         return Nil_Signature;
      end if;
      return Enclosing_Signature (Enclosing_Element (Stmt)) & Stmt;
   end Signature;

   -------------------
   -- Matching_Call --
   -------------------

   function Matching_Call (Stat : Asis.Statement; Signature : Nesting_Signature) return Asis.Statement is
   -- A signature contains only statements (if statements, procedure call), paths (if_path, else_path),
   -- or expressions (from paths conditions)
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Utilities;

      Current : Asis.Element;
   begin
      case Statement_Kind (Stat) is
         when An_If_Statement | A_Procedure_Call_Statement | An_Entry_Call_Statement =>
            Current := Stat;
         when others =>
            return Nil_Element;
      end case;

      for E : Asis.Element of Signature loop
         case Element_Kind (E) is
            when An_Expression =>
               declare
                  Expr : constant Asis.Expression := Condition_Expression (Statement_Paths (Current) (1));
               begin
                  if not Is_Boolean_Constant (Expr) then
                     return Nil_Element;
                  end if;
                  if not Is_Equal (Corresponding_Name_Declaration (Ultimate_Name (E)),
                                   Corresponding_Name_Declaration (Ultimate_Name (Expr)))
                  then
                     return Nil_Element;
                  end if;
               end;

            when A_Statement =>
               case Statement_Kind (E) is
                  when An_If_Statement =>
                     if Statement_Kind (Current) /= An_If_Statement then
                        return Nil_Element;
                     end if;
                  when A_Procedure_Call_Statement | An_Entry_Call_Statement =>
                     if Statement_Kind (Current) /= Statement_Kind (E) then
                        return Nil_Element;
                     end if;
                     return Current;
                  when others =>
                     Failure ("Bad signature: invalid statement", E);
               end case;

            when A_Path =>
               if Statement_Kind (Current) /= An_If_Statement then
                  return Nil_Element;
               end if;
               declare
                  If_Paths : constant Asis.Path_List := Statement_Paths (Current);
               begin
                  case Path_Kind (E) is
                     when An_If_Path =>
                        Current := If_Paths (1);
                     when An_Else_Path =>
                        if If_Paths'Length = 1 or else Path_Kind (If_Paths (2)) /= An_Else_Path then
                           return Nil_Element;
                        end if;
                        Current := If_Paths (2);
                     when others =>
                        Failure ("Bad signature: bad path", E);
                  end case;
               end;
               Current := Sequence_Of_Statements (Current) (1);

            when others =>
               Failure ("Bad signature: unexpected element", E);
         end case;
      end loop;

      return Current;
   end Matching_Call;

   --------------------
   -- Matching_Block --
   --------------------

   function Matching_Block (Stat : Asis.Statement; Signature : Nesting_Signature) return Asis.Statement is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Utilities;

      Current  : Asis.Element;
      Previous : Asis.Element;
   begin
      if Signature'Length = 0 then  -- "signature" of a bad block
         return Nil_Element;
      end if;

      if Signature'Length = 1 then  -- Must be a simple call
         return Stat;
      end if;

      Current := Enclosing_Element (Stat);
      for E : Asis.Element of reverse Signature (Signature'First .. Signature'Last - 1) loop
         case Element_Kind (E) is
            when An_Expression =>
               if Statement_Kind (Current) /= An_If_Statement then
                  return Nil_Element;
               end if;
               declare
                  Expr : constant Asis.Expression := Condition_Expression (Statement_Paths (Current) (1));
               begin
                  if not Is_Boolean_Constant (Expr) then
                     return Nil_Element;
                  end if;
                  if not Is_Equal (Corresponding_Name_Declaration (Ultimate_Name (E)),
                                   Corresponding_Name_Declaration (Ultimate_Name (Expr)))
                  then
                     return Nil_Element;
                  end if;
               end;

            when A_Statement =>
               -- This is an if statement
               if Statement_Kind (Current) /= An_If_Statement then
                  return Nil_Element;
               end if;
               Previous := Current;
               Current  := Enclosing_Element (Current);

            when A_Path =>
               if Path_Kind (E) /= Path_Kind (Current)  then
                  return Nil_Element;
               end if;
               Current := Enclosing_Element (Current);   -- Can't be a path here

            when others =>
               Failure ("Bad signature: unexpected element", E);
         end case;
      end loop;

      return Previous;
   end Matching_Block;

end Rules.Unsafe_Paired_Calls.Services;
