----------------------------------------------------------------------
--  Rules.No_Safe_Initialization - Package body                     --
--                                                                  --
--  This  software  is  (c)  CSEE  and Adalog  2004-2006.  The  Ada --
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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  Binary_Map,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.No_Safe_Initialization is
   use Framework;

   type Object_Kind is (K_Out_Parameter, K_Variable);
   package Object_Kind_Flag_Utilities is new Framework.Language.Flag_Utilities (Object_Kind, "K_");

   type Usage_Flags is array (Object_Kind) of Boolean;

   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;

   type Usage_Contexts is array (Object_Kind) of Basic_Rule_Context;
   Usage : Usage_Contexts;

   type Reference_Kind is (None, Assigned);
   type Object_Information is
      record
         Identifier : Asis.Defining_Name;
         Kind       : Object_Kind;
         Reference  : Reference_Kind;
      end record;

   package Object_Info_Map is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                              Object_Information,
                                              Ada.Strings.Wide_Unbounded."<",
                                              Ada.Strings.Wide_Unbounded.">");

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Object_Kind_Flag_Utilities.Help_On_Flags ("Parameter(s): ");
      User_Message ("Control out parameters and local variables that are not initialized.");
      User_Message ("before the first compound statement");
   end Help;


   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Framework.Language;
      use Object_Kind_Flag_Utilities;

      Key : Object_Kind;
   begin
      if Parameter_Exists then
         Key := Get_Flag_Parameter (Allow_Any => False);

         if Rule_Used (Key) then
            Parameter_Error ("Rule " & Rule_Id &
                             " can be specified only once for each parameter.");
         end if;

         Rule_Used (Key) := True;
         Usage (Key)     := Basic.New_Context (Rule_Use_Type, Label);
      else
         if Rule_Used /= Usage_Flags'(others => False) then
            Parameter_Error ("Rule " & Rule_Id &
                             " can be specified only once for each parameter.");
         end if;

         Rule_Used := Usage_Flags'(others => True);
         Usage     := Usage_Contexts'(others => Basic.New_Context (Rule_Use_Type, Label));
      end if;
   end Add_Use;


   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -----------------------
   -- Process_Structure --
   -----------------------

   procedure Process_Structure (Elem : in Asis.Element) is
      use Ada.Strings.Wide_Unbounded, Object_Info_Map;

      Object_Map : Object_Info_Map.Map;

      procedure Add_Out_Parameters (Element : in Asis.Element) is
         use Asis, Asis.Declarations, Asis.Elements, Asis.Statements;
         use Utilities, Thick_Queries;

         function General_Parameter_Profile (Construct : Asis.Element) return Asis.Parameter_Specification_List is
         begin
            if Statement_Kind (Construct) = An_Accept_Statement then
               return Accept_Parameters (Construct);
            else
               return Parameter_Profile (Construct);
            end if;
         end General_Parameter_Profile;
      begin
         if not Rule_Used (K_Out_Parameter) then
            return;
         end if;

         if Declaration_Kind (Element)        = A_Package_Body_Declaration
           or else Declaration_Kind (Element) = A_Task_Body_Declaration
           or else Statement_Kind (Element)   = A_Block_Statement
         then
            -- These have no parameters
            return;
         end if;

         declare
            Params_Profile : constant Asis.Parameter_Specification_List := General_Parameter_Profile (Element);
         begin
            for Profile_Index in Params_Profile'Range loop
               case Mode_Kind (Params_Profile (Profile_Index)) is
                  when Not_A_Mode =>
                     Failure (Rule_Id & ": Not_A_Mode");
                  when An_Out_Mode =>
                     declare
                        Param_Names : constant Asis.Defining_Name_List := Names (Params_Profile (Profile_Index));
                     begin
                        for Param_Index in Param_Names'Range loop
                           Add (Object_Map,
                                To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Param_Names (Param_Index)))),
                                (Identifier => Param_Names (Param_Index), Kind => K_Out_Parameter, Reference => None));
                        end loop;
                     end;
                  when others =>
                     null;
               end case;
            end loop;
         end;
      end Add_Out_Parameters;

      procedure Add_Variables (Element : in Asis.Element) is
         use Asis, Asis.Declarations, Asis.Elements;
         use Thick_Queries, Utilities;
      begin
         if not Rule_Used (K_Variable) then
            return;
         end if;

         if Statement_Kind (Element) = An_Accept_Statement then
            -- No declarations
            return;
         end if;

         if Declaration_Kind (Element) = A_Package_Body_Declaration then
            if Is_Subunit (Element) then
               Add_Variables (Corresponding_Declaration (Corresponding_Body_Stub (Element)));
            else
               Add_Variables (Corresponding_Declaration (Element));
            end if;
         end if;

         declare
            Decls : constant Asis.Declaration_List := Declarative_Items (Element);
         begin
            for Decl_Index in Decls'Range loop
               if Declaration_Kind (Decls (Decl_Index)) = A_Variable_Declaration
                 and then Is_Nil (Initialization_Expression (Decls (Decl_Index)))
               then
                  declare
                     Var_Names: constant Asis.Defining_Name_List := Names (Decls (Decl_Index));
                  begin
                     for Var_Index in Var_Names'Range loop
                        Add (Object_Map,
                             To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Var_Names (Var_Index)))),
                             (Identifier => Var_Names (Var_Index), Kind => K_Variable, Reference  => None));
                     end loop;
                  end;
               end if;
            end loop;
         end;
      end Add_Variables;

      procedure Update (Name : Asis.Expression) is
         use Asis, Asis.Elements, Asis.Expressions;
         use Thick_Queries, Utilities;

         Good_Name : Asis.Expression := Name;
         Info      : Object_Information;
      begin
         loop
            case Expression_Kind (Good_Name) is
               when An_Identifier =>
                  -- Retrieve the assigned variable definition
                  Good_Name := Ultimate_Name (Good_Name);
                  if Is_Nil (Good_Name) then
                     -- Renaming of something dynamic, ignore
                     return;
                  end if;
                  exit;

               when A_Selected_Component =>
                  Good_Name := Selector (Good_Name);

               when A_Type_Conversion =>
                  Good_Name := Converted_Or_Qualified_Expression (Good_Name);

               when A_Slice
                 | An_Indexed_Component
                 | An_Explicit_Dereference
                 =>
                  -- Assignment to part of a variable, ignore
                  return;

               when others =>
                  Failure (Rule_Id & ": invalid expression kind", Good_Name);
            end case;
         end loop;

         case Declaration_Kind (Corresponding_Name_Declaration (Good_Name)) is
            when A_Variable_Declaration | A_Parameter_Specification =>
               declare
                  Name_Image : constant Unbounded_Wide_String
                    := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Good_Name)));
               begin
                  if Is_Present (Object_Map, Name_Image) then
                     Info           := Fetch (Object_Map, Name_Image);
                     Info.Reference := Assigned;
                     Add (Object_Map, Name_Image, Info);
                  end if;
               end;
            when others =>
               -- A record component or protected component...
               null;
         end case;
      end Update;

      procedure Report_One (Key : Unbounded_Wide_String; Info : in out Object_Information) is
         pragma Unreferenced (Key);
         use Asis.Declarations;
         use Framework.Reports;
      begin
         if Info.Reference = None then
            case Info.Kind is
               when K_Out_Parameter =>
                  Report (Rule_Id,
                          Usage (K_Out_Parameter),
                          Get_Location (Info.Identifier),
                          "out parameter """ & Defining_Name_Image (Info.Identifier)
                            & """ not safely initialized");
               when K_Variable =>
                  Report (Rule_Id,
                          Usage (K_Variable),
                          Get_Location (Info.Identifier),
                          "variable """ & Defining_Name_Image (Info.Identifier)
                            & """ not safely initialized");
            end case;
         end if;
      end Report_One;

      procedure Report_All is new Iterate (Report_One);

      use Thick_Queries, Utilities;
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
   begin -- Process_Structure

      if Rule_Used = Usage_Flags'(others => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Add_Out_Parameters (Elem);
      Add_Variables      (Elem);

      declare
         Statement_List : constant Asis.Statement_List := Thick_Queries.Statements (Elem);
      begin

         -- Update references from variables and out parameters assignments
         for Stmt_Index in Statement_List'range loop
            case Statement_Kind (Statement_List (Stmt_Index)) is
               when An_Assignment_Statement =>
                  Update (Assignment_Variable_Name (Statement_List (Stmt_Index)));

               when An_Entry_Call_Statement | A_Procedure_Call_Statement =>
                  -- Check for out parameters in procedure and entry calls
                  declare
                     Actuals : constant Asis.Association_List
                       := Call_Statement_Parameters (Statement_List (Stmt_Index));
                     Formal  : Asis.Defining_Name;
                  begin
                     for Actual_Index in Actuals'Range loop
                        Formal := Formal_Name (Statement_List (Stmt_Index), Actual_Index);
                        -- Formal is nil for calls to a dispatching operation
                        -- We don't know the mode => pretend we do nothing
                        -- (consistent with the fact that dispatching calls are ignored)
                        if not Is_Nil (Formal) then
                           case Mode_Kind (Enclosing_Element (Formal)) is
                              when Not_A_Mode =>
                                 Failure (Rule_Id & ": Not_A_Mode");
                              when An_Out_Mode =>
                                 Update (Actual_Parameter (Actuals (Actual_Index)));
                              when others =>
                                 null;
                           end case;
                        end if;
                     end loop;
                  end;

               when others =>
                  -- End of intialization statements
                  exit;
            end case;
         end loop;

        Report_All (Object_Map);
      end;

      Clear (Object_Map);
   exception
      when others =>
         -- Prevent memory leak in case of problem
         Clear (Object_Map);
         raise;
   end Process_Structure;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.No_Safe_Initialization;
