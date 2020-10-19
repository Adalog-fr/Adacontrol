----------------------------------------------------------------------
--  Rules.Style - Package specification                             --
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

-- Asis
with
  Asis;

-- Adactl
private with
   Framework,
   Framework.Control_Manager;

package Rules.Style is
   Rule_Id : constant Wide_String := "STYLE";

   procedure Process_Aspect             (Aspect      : in Asis.Definition);
   procedure Process_Attribute          (Attribute   : in Asis.Expression);
   procedure Process_Compound_Statement (Statement   : in Asis.Statement);
   procedure Process_Construct          (Construct   : in Asis.Declaration);
   procedure Process_Declaration        (Declaration : in Asis.Declaration);
   procedure Process_Element            (Element     : in Asis.Element);
   procedure Process_Identifier         (Identifier  : in Asis.Expression);
   procedure Process_Literal            (Expression  : in Asis.Expression);
   procedure Process_Pragma             (Pr          : in Asis.Pragma_Element);
   procedure Process_Renaming           (Ren         : in Asis.Declaration);
   -- Process_Renaming is plugged as Post_Procedure, since the occurrence of an identifier
   -- is still allowed within the renaming itself.
   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Locations.Location);

private
   -- These declarations here to be visible from child

   -- Subrules for the rule
   -- "casing" subrules must stay together
   -- "parameter_order" subrules must stay together
   type Subrules is (St_Casing_Aspect,      St_Casing_Attribute,       St_Casing_Exponent,  -- casing subrules
                     St_Casing_Identifier,  St_Casing_Keyword,         St_Casing_Number,
                     St_Casing_Pragma,

                     St_Compound_Statement, St_Default_In,      St_Exposed_Literal,
                     St_Multiple_Elements,  St_No_Closing_Name, St_Numeric_Literal,

                     St_Parameter_Order,    St_Formal_Parameter_Order,                      -- parameter_order subrules

                     St_Renamed_Entity);
   subtype St_Orders is Subrules range St_Parameter_Order .. St_Formal_Parameter_Order;

   type Casing_Names is (Ca_Uppercase, Ca_Lowercase, Ca_Titlecase, Ca_Original);
   -- Ca_Original must stay last, assumed in Check_Casing
   type Casing_Set   is array (Casing_Names) of Boolean;

   function Corresponding_Context (Subrule    : Subrules;
                                   Complement : Wide_String := "")
                                   return Framework.Control_Manager.Root_Context'Class;
   function Should_Be (Source   : Wide_String;
                       Expected : Casing_Set;
                       Original : Wide_String := "";
                       For_Fix  : Boolean)
                       return Wide_String;
   -- if For_Fix is True, return a single possible value, without quotes.
   -- if For_Fix is Fales, return all allowed value, within quotes, separated by " or "
end Rules.Style;
