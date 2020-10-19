----------------------------------------------------------------------
--  Rules.Object_Declarations.NRT_Utilities - Package specification --
--                                                                  --
--  This software is (c) Adalog 2004-2018.                          --
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

-- AdaControl
with
  Framework.Control_Manager,
  Framework.Symbol_Table;

package Rules.Object_Declarations.NRT_Utilities is

   type Variable_State is (Suspect, Required);
   package Active_Suspect_Variables is new Framework.Symbol_Table.Data_Access (Variable_State);


   function Subtype_Or_Type_Context (Store   : Framework.Control_Manager.Context_Store;
                                     St_Name : Asis.Element)
                                     return Framework.Control_Manager.Root_Context'Class;
   -- Returns the context associated with the subtype St_Name, or its type if not found for subtype.
   -- Expected elements:
   --   A_Defining_Name
   --   A_Name

   procedure Make_Required (Ident : Asis.Element);
   -- Declare that Ident is a required variable

   procedure Make_Dependent (Obj : Asis.Name; On : Asis.Name);
   -- Declare that Obj depends on On

   procedure Check_Not_Required (Name : Asis.Name);
   -- Check if Name is required, and report if it isn't

   function Is_Requiring (Expr : Asis.Expression; Of_Type : Asis.Declaration) return Boolean;
   -- Checks if Expr is a requiring expression for Of_Type

   procedure Make_Suspect (Name      : Asis.Name;
                           Cont      : Framework.Control_Manager.Basic_Rule_Context;
                           Type_Name : Wide_String);
   -- Declare that Name is a suspect object for type Type_Name, with rule context Cont

   function Is_Suspect (Ident : Asis.Name) return Boolean;
   -- Returns True if Ident is a suspect object

end Rules.Object_Declarations.NRT_Utilities;
