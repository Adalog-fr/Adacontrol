----------------------------------------------------------------------
--  Framework.Specific_Plugs - Package body                         --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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
  Asis.Elements;

package body Framework.Specific_Plugs is

   ----------------
   -- Enter_Unit --
   ----------------

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      pragma Unreferenced (Unit);
   begin
      null;
   end Enter_Unit;

   --------------------------
   -- Exit_Context_Clauses --
   --------------------------

   procedure Exit_Context_Clauses (Unit : in Asis.Compilation_Unit) is
      pragma Unreferenced (Unit);
   begin
      null;
   end Exit_Context_Clauses;

   ---------------
   -- Exit_Unit --
   ---------------

   procedure Exit_Unit (Unit : in Asis.Compilation_Unit) is
      pragma Unreferenced (Unit);
   begin
      null;
   end Exit_Unit;

   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope (Element : in Asis.Element) is
      pragma Unreferenced (Element);
   begin
      null;
   end Enter_Scope;

   ------------------------
   -- Enter_Private_Part --
   ------------------------

   procedure Enter_Private_Part   (Element : in Asis.Element) is
      pragma Unreferenced (Element);
   begin
      null;
   end Enter_Private_Part;

   ----------------
   -- Exit_Scope --
   ----------------

   procedure Exit_Scope (Element : in Asis.Element) is
      pragma Unreferenced (Element);
   begin
      null;
   end Exit_Scope;

   --------------------------
   -- Enter_Statement_List --
   --------------------------

   procedure Enter_Statement_List (Element : in Asis.Element) is
      pragma Unreferenced (Element);
   begin
      null;
   end Enter_Statement_List;

   ---------------------
   -- True_Identifier --
   ---------------------

   procedure True_Identifier (Element : in Asis.Expression) is
      pragma Unreferenced (Element);
   begin
      null;
   end True_Identifier;

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element : in Asis.Element) is
      use Asis.Elements;
   begin
      case Element_Kind (Element) is
         when others =>
            null;
      end case;
   end Pre_Procedure;

   --------------------
   -- Post_Procedure --
   --------------------

   procedure Post_Procedure (Element : in Asis.Element) is
      use Asis.Elements;
   begin
      case Element_Kind (Element) is
         when others =>
            null;
      end case;
   end Post_Procedure;

   ---------------------
   -- Text_Enter_Unit --
   ---------------------

   procedure Text_Enter_Unit (Unit : in Asis.Compilation_Unit) is
      pragma Unreferenced (Unit);
   begin
      null;
   end Text_Enter_Unit;

   -------------------
   -- Text_Analysis --
   -------------------

   procedure Text_Analysis (Line : Asis.Program_Text; Loc : Location) is
      pragma Unreferenced (Line, Loc);
   begin
      null;
   end Text_Analysis;

end Framework.Specific_Plugs;
