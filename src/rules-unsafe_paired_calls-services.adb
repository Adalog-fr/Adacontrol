----------------------------------------------------------------------
--  Rules.Unsafe_Paired_Calls.Signatures - Package body             --
--                                                                  --
--  This module is  (c) BelgoControl and Adalog  2004-2005. The Ada --
--  Controller  is  free software;  you can redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
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
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
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

   ------------------------------------
   -- Effective_Last_Statement_Index --
   ------------------------------------

   function Effective_Last_Statement_Index (Stats : Asis.Statement_List) return Asis.List_Index is
      use Asis, Asis.Elements;
      use Utilities;
   begin
      for S in reverse Stats'Range loop
         case Statement_Kind (Stats (S)) is
            when A_Return_Statement | An_Exit_Statement | A_Null_Statement =>
               null;
            when others =>
               return S;
         end case;
      end loop;

      -- Statement list contains only return, exit and null statements...
      -- we shouldn't be here!
      Failure ("No effective statement in statements list");
   end Effective_Last_Statement_Index;


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

   ------------------------------
   -- Effective_Last_Statement --
   ------------------------------

   function Effective_Last_Statement (Stats : Asis.Statement_List) return Asis.Statement is
   begin
      return Stats (Effective_Last_Statement_Index (Stats));
   end Effective_Last_Statement;


   ---------------
   -- Signature --
   ---------------

   function Signature (Stmt : Asis.Statement) return Nesting_Signature is
      use Asis.Elements;

      function Enclosing_Signature (Elem : Asis.Element) return Nesting_Signature is
         use Asis, Asis.Statements;
      begin
         case Element_Kind (Elem) is
            when A_Statement =>
               case Statement_Kind (Elem) is
                  when An_If_Statement =>
                     if Conditionals_Allowed.Value = Off then
                        raise Invalid_Nesting with "nested calls not allowed";
                     end if;
                     declare
                        Expr : constant Asis.Expression := Condition_Expression (Statement_Paths (Elem) (1));
                     begin
                        if not Is_Boolean_Constant (Expr) then
                           raise Invalid_Nesting with "if condition is not a boolean constant";
                        end if;
                        return Enclosing_Signature (Enclosing_Element (Elem)) & Elem & Expr;
                     end;
                  when others =>
                     return Nil_Signature;
               end case;

            when A_Path =>
               case Path_Kind (Elem) is
                  when An_If_Path | An_Else_Path =>
                     -- Only one non effective statement allowed, must be the first statement
                     declare
                        Stats : constant Asis.Statement_List := Thick_Queries.Statements (Elem);
                     begin
                        if Effective_Last_Statement_Index (Stats) /= Stats'First then
                           raise Invalid_Nesting with "path contains disallowed statements";
                        end if;
                     end;
                     return Enclosing_Signature (Enclosing_Element (Elem)) & Elem;
                  when others =>
                     raise Invalid_Nesting with "call in disallowed structured statement";
               end case;

            when others =>
               return Nil_Signature;
         end case;
      end Enclosing_Signature;

   begin   -- Signature
      return Enclosing_Signature (Enclosing_Element (Stmt)) & Stmt;
   end Signature;

   -------------------
   -- Matching_Call --
   -------------------

   function Matching_Call (Stat : Asis.Statement; Signature : Nesting_Signature) return Asis.Statement is
   -- A signature contains only statements (if statements, procedure call) or paths (if_path, else_path)
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Utilities;

      Current : Asis.Element := Stat;
   begin
      case Statement_Kind (Stat) is
         when An_If_Statement | A_Procedure_Call_Statement | An_Entry_Call_Statement =>
            null;
         when others =>
            return Nil_Element;
      end case;

      for E in Signature'Range loop
         case Element_Kind (Signature (E)) is
            when An_Expression =>
               declare
                  Expr : constant Asis.Expression := Condition_Expression (Statement_Paths (Current) (1));
               begin
                  if not Is_Boolean_Constant (Expr) then
                     return Nil_Element;
                  end if;
                  if not Is_Equal (Corresponding_Name_Declaration (Ultimate_Name (Signature (E))),
                                   Corresponding_Name_Declaration (Ultimate_Name (Expr)))
                  then
                     return Nil_Element;
                  end if;
               end;

            when A_Statement =>
               case Statement_Kind (Signature (E)) is
                  when An_If_Statement =>
                     if Statement_Kind (Current) /= An_If_Statement then
                        return Nil_Element;
                     end if;
                  when A_Procedure_Call_Statement | An_Entry_Call_Statement =>
                     if Statement_Kind (Current) /= Statement_Kind (Signature (E)) then
                        return Nil_Element;
                     end if;
                  when others =>
                     Failure ("Bad signature: not a call statement", Signature (E));
               end case;

            when A_Path =>
               declare
                  If_Paths : constant Asis.Path_List := Statement_Paths (Current);
               begin
                  case Path_Kind (Signature (E)) is
                     when An_If_Path =>
                        Current := If_Paths (1);
                     when An_Else_Path =>
                        if If_Paths'Length = 1 or else Path_Kind (If_Paths (2)) /= An_Else_Path then
                           return Nil_Element;
                        end if;
                        Current := If_Paths (2);
                     when others =>
                        Failure ("Bad signature: bad path", Signature (E));
                  end case;
               end;
               Current := Sequence_Of_Statements (Current) (1);

            when others =>
               Failure ("Bad signature: unexpected element", Signature (E));
         end case;
      end loop;

      return Current;
   end Matching_Call;

end Rules.Unsafe_Paired_Calls.Services;
