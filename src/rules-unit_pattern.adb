----------------------------------------------------------------------
--  Rules.Unit_Pattern - Package body                               --
--                                                                  --
--  This software  is (c) Adalog/Alstom  2004-2013.                 --
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
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
----------------------------------------------------------------------

-- ASIS
with
   Asis.Compilation_Units,
   Asis.Declarations,
   Asis.Definitions,
   Asis.Elements,
   Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;
pragma Elaborate (Framework.Language);

package body Rules.Unit_Pattern is
   use Framework, Framework.Control_Manager;

   -- Algorithm:
   --
   -- Easy for Single_Tagged_Type, Tagged_Type_Hierarchy, Context_Clauses_Order

   type Subrules is (Single_Tagged_Type, Tagged_Type_Hierarchy, Context_Clauses_Order);
   type Subrules_Set is array (Subrules) of Boolean;
   Empty_Set : constant Subrules_Set := (others => False);

   package Patterns_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules);
   use Patterns_Flags_Utilities;

   Rule_Used : Subrules_Set := Empty_Set;
   Save_Used : Subrules_Set;
   Contexts  : array (Subrules) of Basic_Rule_Context;

   ---- Declarations for Single_Tagged type
   --                    ******************

   package Declarations_Store is new Framework.Scope_Manager.Scoped_Store (Asis.Declaration);


   ---- Declarations for Tagged_Type_Hierarchy
   --                    *********************

   -- (none)


   ---- Declarations for Context_Clauses_Order
   --                    *********************

   type Context_Clause_Element is (CC_With, CC_Use, CC_Use_Type, CC_Pragma);
   package Context_Clause_Element_Utilities is new Framework.Language.Modifier_Utilities
                                                     (Modifiers => Context_Clause_Element,
                                                      Prefix    => "CC_");
   type Order_Index is range 1 .. Context_Clause_Element'Pos (Context_Clause_Element'Last) + 1 + 1;
   -- Number of orders that can be specified.
   -- Quite arbitrary; here we allow one position for each Context_Clause_Element, plus the extra
   -- guard value.
   type Clauses_Array is array (Order_Index) of Context_Clause_Element_Utilities.Modifier_Set;
   Clauses_Order : Clauses_Array;
   Order_Inx     : Order_Index;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls various usage patterns of units and entities in them");
      User_Message;
      Help_On_Flags ("Parameter (1):");
      User_Message ("For Context_Clauses_Order:");
      Context_Clause_Element_Utilities.Help_On_Modifiers(Header => "   parameter (2..): list of");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;

      Subrule : Subrules;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "This rule requires at least one parameter");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);
      if Rule_Used (Subrule) then
         Parameter_Error (Rule_Id, "This rule can be given only once");
      end if;

      case Subrule is
         when Single_Tagged_Type | Tagged_Type_Hierarchy =>
            null;
         when Context_Clauses_Order =>
            if not Parameter_Exists then
               Parameter_Error (Rule_Id, "At least one clause set must be specified");
            end if;

            declare
               use Context_Clause_Element_Utilities;
               Not_Specified : Modifier_Set := Full_Set;
            begin
               Order_Inx := 1;
               loop
                  Clauses_Order (Order_Inx) := Get_Modifier_Set (No_Parameter => True);
                  Not_Specified := Not_Specified and not Clauses_Order (Order_Inx);
                  exit when not Parameter_Exists;
                  if Order_Inx = Order_Index'Last - 1 then
                     Parameter_Error (Rule_Id, "Too many parameters");
                  end if;
                  Order_Inx := Order_Inx + 1;
               end loop;
               if Not_Specified /= Context_Clause_Element_Utilities.Empty_Set then
                  -- allow all clauses not explicitely specified after the ones specified
                        Order_Inx := Order_Inx + 1;
                        Clauses_Order (Order_Inx) := Not_Specified;
               end if;
            end;
      end case;

      Rule_Used (Subrule) := True;
      Contexts  (Subrule) := Basic.New_Context (Ctl_Kind, Ctl_Label);
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := Empty_Set;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Empty_Set;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Declarations_Store.Activate;
   end Prepare;


   ------------------------------
   -- Check_Single_Tagged_Type --
   ------------------------------

   procedure Check_Single_Tagged_Type (Declaration : in Asis.Declaration) is
      -- Precondition: Declaration is for a tagged type
      use Asis, Asis.Elements;
      use Framework.Reports, Framework.Scope_Manager;
   begin
      if not Rule_Used (Single_Tagged_Type) then
         return;
      end if;

      case Declaration_Kind (Current_Scope) is
         when A_Package_Declaration
            | A_Generic_Package_Declaration
            | A_Package_Body_Declaration
            =>
            Declarations_Store.Reset (Current_Scope_Only);
            if Declarations_Store.Data_Available then
               Report (Rule_Id,
                       Contexts (Single_Tagged_Type),
                       Get_Location (Declaration),
                       "Tagged type already declared at " & Image (Get_Location (Declarations_Store.Current_Data)));
            else
               Declarations_Store.Push (Declaration);
            end if;
         when others =>
            null;
      end case;
   end Check_Single_Tagged_Type;


   ---------------------------------
   -- Check_Tagged_Type_Hierarchy --
   ---------------------------------

   procedure Check_Tagged_Type_Hierarchy (Declaration : in Asis.Declaration) is
   -- Precondition: Declaration is for a (full) type extension
      use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Thick_Queries;
   begin
      if not Rule_Used (Tagged_Type_Hierarchy) then
         return;
      end if;

      declare
         This_Unit   : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Declaration);
         Parent_Unit : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Corresponding_Name_Declaration
                                                                                     (Subtype_Simple_Name
                                                                                      (Parent_Subtype_Indication
                                                                                       (Type_Declaration_View
                                                                                        (Declaration)))));
      begin
         if Unit_Kind (This_Unit) in A_Subunit   -- Certainly not an appropriate child
           or else not Is_Equal (Corresponding_Parent_Declaration (This_Unit), Parent_Unit)
         then
            Report (Rule_Id,
                    Contexts (Tagged_Type_Hierarchy),
                    Get_Location (Declaration),
                    "Type extension not in child unit of its parent's unit");
         end if;
      end;
   end Check_Tagged_Type_Hierarchy;


   ------------------------------
   -- Process_Type_Declaration --
   ------------------------------

   procedure Process_Type_Declaration (Declaration : in Asis.Declaration) is
      -- Precondition: Declaration_Kind (Declaration) = An_Ordinary_Type_Declaration
      use Asis, Asis.Declarations, Asis.Elements;
   begin   -- Process_Type_Declaration
      if Rule_Used = Empty_Set then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Type_Kind (Type_Declaration_View (Declaration)) is
         when A_Tagged_Record_Type_Definition =>
            Check_Single_Tagged_Type(Declaration);
         when A_Derived_Record_Extension_Definition =>
            Check_Single_Tagged_Type    (Declaration);
            Check_Tagged_Type_Hierarchy (Declaration);
         when others =>
            null;
      end case;
   end Process_Type_Declaration;


   ----------------
   -- Enter_Unit --
   ----------------

   procedure Process_Unit (Unit : in Asis.Compilation_Unit) is
      use Asis, Asis.Elements;
      use Framework.Reports, Utilities;
   begin
      if not Rule_Used (Context_Clauses_Order) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Context_Elements : constant Asis.Element_List := Context_Clause_Elements (Unit, Include_Pragmas => True);
         State            : Order_Index := Order_Index'First;
         Old_State        : Order_Index;
         Clause           : Context_Clause_Element;
      begin
         for C in Context_Elements'Range loop
            case Element_Kind (Context_Elements (C)) is
               when A_Clause =>
                  case Clause_Kind (Context_Elements (C)) is
                     when A_Use_Package_Clause =>
                        Clause := CC_Use;
                     when A_Use_Type_Clause | A_Use_All_Type_Clause =>
                        Clause := CC_Use_Type;
                     when A_With_Clause =>
                        Clause := CC_With;
                     when others =>
                        Failure ("Enter_Unit: unexpected clause", Context_Elements (C));
                  end case;

               when A_Pragma =>
                  Clause := CC_Pragma;

               when others =>
                  Failure ("Enter_Unit: unexpected clause", Context_Elements (C));
            end case;

            Old_State := State;
            while not Clauses_Order (State) (Clause) loop
               if State = Order_Inx then
                  Report (Rule_Id,
                          Contexts (Context_Clauses_Order),
                          Get_Location (Context_Elements (C)),
                          "clause (or pragma) out of order");
                  -- Avoid multiple messages if there was only one clause out of order:
                  State := Old_State;
                  exit;
               end if;

               State := State + 1;
            end loop;
         end loop;
      end;
   end Process_Unit;

begin  -- Rules.Unit_Pattern
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Unit_Pattern;
