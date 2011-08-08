----------------------------------------------------------------------
--  Rules.Reduceable_Scope - Package body                           --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
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
  Ada.Unchecked_Deallocation,
  Ada.Strings.Wide_Unbounded;


-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;
pragma Elaborate (Framework.Language);

package body Rules.Reduceable_Scope is
   use Framework, Framework.Scope_Manager, Utilities;

   -- Algorithm:
   -- We use a Scoped_Store to maintain a list of declared entities.
   -- Each entry consists of the defining name of the entity, plus a path to where
   -- the declaration could be moved. This path is the list of scopes between the scope
   -- of the declaration (not included) and the scope of a reference to the entity (included).
   --
   -- When a reference is encountered, the corresponding path is compared to the one
   -- stored with the entity. If they are unequal, only the common part of both paths
   -- is kept. If there is no such common part, the entity cannot be moved and is removed
   -- from the store.
   --
   -- At scope exit, remainining entities can be moved, and the top of the associated path
   -- tells where.

   type Restriction_Flags is (Restr_No_Blocks);
   package Restriction_Flag_Utilities is new Framework.Language.Flag_Utilities (Restriction_Flags, "RESTR_");

   Rule_Used  : Boolean := False;
   Save_Used  : Boolean;
   Rule_Type  : Rule_Types;
   Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   No_Blocks  : Boolean := False;

   -- Management of declaration information
   type Scope_List_Access is access Scope_List;
   procedure Free is new Ada.Unchecked_Deallocation (Scope_List, Scope_List_Access);

   type Declaration_Info is
      record
         Elem : Asis.Element;
         Path : Scope_List_Access;
      end record;

   function Equivalent_Keys (L, R : Declaration_Info) return Boolean;
   package Local_Declarations is new Scoped_Store (Declaration_Info, Equivalent_Keys);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Restriction_Flag_Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter: ", Footer => "(optional)");
      User_Message  ("Control declarations that could be moved to an inner scope,");
      User_Message  ("I.e. where all references are from a single nested scope");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language, Restriction_Flag_Utilities;
      Restriction : Restriction_Flags;
   begin
      if Rule_Used then
         Parameter_Error (Rule_Id, "this rule can be specified only once");
      end if;

      if  Parameter_Exists then
         Restriction := Get_Flag_Parameter (Allow_Any => False);
         case Restriction is
            when Restr_No_Blocks=>
               No_Blocks := True;
         end case;
      end if;

      Rule_Type  := Rule_Use_Type;
      Rule_Label := To_Unbounded_Wide_String (Label);
      Rule_Used  := True;
   end Add_Use;


   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := False;
            Rule_Label := Null_Unbounded_Wide_String;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (L, R : Declaration_Info) return Boolean is
      use Asis.Elements;
   begin
      return Is_Equal (L.Elem, R.Elem);
   end Equivalent_Keys;


   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Def: in Asis.Defining_Name) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      Enclosing_Unit : Asis.Declaration;
      Enclosing_Decl : Asis.Declaration;
      Temp           : Asis.Element;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Enclosing_Decl  := Enclosing_Element (Def);
      -- Make sure we have really the declaration
      -- (case of defining expanded names of child units)
      while Defining_Name_Kind (Enclosing_Decl) = A_Defining_Expanded_Name loop
         Enclosing_Decl := Enclosing_Element (Enclosing_Decl);
      end loop;

      if Is_Nil (Enclosing_Element (Enclosing_Decl)) then
         -- This is the defining name of a compilation unit
         return;
      end if;

      case Element_Kind (Enclosing_Decl) is
         when A_Declaration =>
            case Declaration_Kind (Enclosing_Decl) is
               when A_Component_Declaration
                 | A_Discriminant_Specification
                 | A_Choice_Parameter_Specification
                 | A_Parameter_Specification
                 | A_Formal_Declaration
                 | A_Loop_Parameter_Specification
                 | A_Single_Task_Declaration
                 =>
                  -- Things that cannot be moved:
                  --   Components of structured data
                  --   Identifiers of exception handlers
                  --   (generic) formal parameters
                  --   Control variables of for loops
                  --   Task objects (since it would change the master)
                  return;
               when A_Variable_Declaration =>
                  Temp := Object_Declaration_View (Enclosing_Decl);
                  if Definition_Kind (Temp) = A_Subtype_Indication then
                     Temp := Subtype_Simple_Name (Temp);
                     if Expression_Kind (Temp) /= An_Attribute_Reference then
                        -- 'Base is not applicable to a task type, nor 'Class (for the moment!)
                        if Is_Type_Declaration_Kind (Corresponding_Name_Declaration (Temp),
                                                     A_Task_Type_Declaration)
                        then
                           return;
                        end if;
                     end if;
                  end if;

                  if not Is_Nil (Initialization_Expression (Enclosing_Decl)) then
                     -- Consider that initialization is a reference from same scope
                     return;
                  end if;
               when A_Constant_Declaration
                 | An_Integer_Number_Declaration
                 | A_Real_Number_Declaration
                 =>
                  -- Since these are always initialized...
                  return;
               when A_Procedure_Body_Declaration
                 | A_Function_Body_Declaration
                 =>
                  -- Do not consider the body if there is an explicit spec.
                  -- since in this case, we use the defining name from the spec.
                  if not Is_Nil (Corresponding_Declaration (Enclosing_Decl)) then
                     return;
                  end if;
               when A_Package_Body_Declaration
                 | A_Task_Body_Declaration
                 | A_Protected_Body_Declaration
                 =>
                  -- These things always have an explicit spec => no need to consider
                  -- the name from the body
                  return;
               when others =>
                  null;
            end case;

         when A_Statement =>
            -- Is Def a statement label?
            declare
               use Asis.Statements;
               Labels : constant Asis.Defining_Name_List := Label_Names (Enclosing_Decl);
            begin
               for I in Labels'Range loop
                  if Is_Equal (Def, Labels (I)) then
                     return;
                  end if;
               end loop;
            end;

            case Statement_Kind (Enclosing_Decl) is
               when A_Loop_Statement
                 | A_For_Loop_Statement
                 | A_While_Loop_Statement
                 | A_Block_Statement
                 =>
                  -- This a loop or block name, cannot be moved
                  return;
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;

      Enclosing_Unit := Enclosing_Element (Enclosing_Decl);
      if Element_Kind (Enclosing_Unit) = A_Definition then
         Enclosing_Unit := Enclosing_Element (Enclosing_Unit);
      end if;
      case Declaration_Kind (Enclosing_Unit) is
         when A_Package_Declaration
           | A_Generic_Package_Declaration
           | A_Task_Type_Declaration
           | A_Single_Task_Declaration
           | A_Protected_Type_Declaration
           | A_Single_Protected_Declaration
           =>
            -- Never process declarations from package specs, task specs and protected specs
            return;
         when others =>
            null;
      end case;

      if Is_Equal (Enclosing_Decl, Current_Scope) then
         -- This is the defining name for the current scope
         -- => it belongs to the enclosing scope
         Local_Declarations.Push_Enclosing ((Elem => Def, Path => null));
      else
         Local_Declarations.Push ((Elem => Def, Path => null));
      end if;
   end Process_Defining_Name;


   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Name : in Asis.Name) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Name_Def   : Asis.Definition := Corresponding_Name_Definition (Name);
         Decl_Level : Scope_Range;
         Info       : Declaration_Info;
         Enclosing  : Asis.Expression;

         procedure Set_Info_Path (Path : Scope_List) is
            -- The path must be trimmed of:
            -- - top scopes that cannot hold any declaration (for loops, exception handlers, accept statements)
            -- - generic instantiations
            Top : Scope_Range := Path'Last;
         begin
            Free (Info.Path); -- OK if Info.Path is null

            while Statement_Kind (Path (Top)) = A_For_Loop_Statement
              or else Statement_Kind (Path (Top)) = An_Accept_Statement
              or else (Statement_Kind (Path (Top)) = A_Block_Statement and No_Blocks)
              or else Element_Kind (Path (Top)) = An_Exception_Handler
              or else Declaration_Kind (Path (Top)) in A_Generic_Instantiation
            loop
               if Top = Path'First then
                  -- Nothing left => remove declaration
                  Local_Declarations.Delete_Current;
                  return;
               else
                  Top := Top - 1;
               end if;
            end loop;

            Info.Path := new Scope_List'(Path (Path'First .. Top));
            Local_Declarations.Update_Current (Info);
         end Set_Info_Path;
      begin
         if Is_Nil (Name_Def) then
            -- Some predefined stuff
            return;
         end if;

         -- If the name returned by Corresponding_Name_Defition is from a body with an
         -- explicit specification, take the name from the spec
         case Declaration_Kind (Enclosing_Element (Name_Def)) is
            when A_Function_Body_Declaration
              | A_Function_Renaming_Declaration
              | A_Function_Body_Stub
              | A_Package_Body_Declaration
              | A_Package_Body_Stub
              | A_Procedure_Body_Declaration
              | A_Procedure_Renaming_Declaration
              | A_Procedure_Body_Stub
              | A_Task_Body_Declaration
              | A_Task_Body_Stub
              | A_Protected_Body_Declaration
              | A_Protected_Body_Stub
              | A_Formal_Package_Declaration
              | A_Formal_Package_Declaration_With_Box
              | A_Generic_Package_Renaming_Declaration
              | A_Generic_Procedure_Renaming_Declaration
              | A_Generic_Function_Renaming_Declaration
              | An_Entry_Body_Declaration
              =>
               if not Is_Nil (Corresponding_Declaration (Enclosing_Element (Name_Def))) then
                  Name_Def := Names (Corresponding_Declaration (Enclosing_Element (Name_Def))) (1);
               end if;
            when others =>
               null;
         end case;

         Local_Declarations.Reset ((Elem => Name_Def, Path => null), Current_Scope_Only);
         if not Local_Declarations.Data_Available then
            -- not found
            return;
         end if;

         Info       := Local_Declarations.Current_Data;
         Decl_Level := Local_Declarations.Current_Data_Level;

         Enclosing := Enclosing_Element (Name);
         if Expression_Kind (Enclosing) = An_Attribute_Reference
           and then A4G_Bugs.Attribute_Kind (Enclosing) in An_Access_Attribute .. An_Address_Attribute
         then
            -- Name used in 'Access or 'Address, too dangerous to move
            Free (Info.Path);
            Local_Declarations.Delete_Current;
            return;
         end if;

         declare
            Current_Path : constant Scope_List := Active_Scopes (Decl_Level+1 .. Current_Depth);
            -- Invariant: Current_Path'First = Info.Path'First = Decl_Level+1
         begin
            if Current_Path'Length = 0 then
               -- Reference from same level as declaration
               -- This declaration cannot be moved. Remove it.
               Free (Info.Path);
               Local_Declarations.Delete_Current;
               return;
            end if;

            if Info.Path = null then
               -- First reference
               Set_Info_Path (Current_Path);
               return;
            end if;

            if not Is_Equal (Current_Path (Current_Path'First), Info.Path (Info.Path'First)) then
               -- Nothing in common, declaration cannot be moved. Remove it.
               Free (Info.Path);
               Local_Declarations.Delete_Current;
               return;
            end if;

            for I in Scope_Range range Current_Path'First + 1 .. Current_Path'Last loop
               if I > Info.Path'Last then
                  -- Info.Path is shorter and matches the beginning of Current_Path
                  -- => keep it
                  return;
               end if;

               if not Is_Equal (Current_Path (I), Info.Path (I)) then
                  -- Keep in Info.Path the common part only
                  Set_Info_Path (Current_Path (Current_Path'First .. I-1));
                  return;
               end if;
            end loop;

            -- Here, Current_Path is either equal to Info.Path or shorter.
            -- In the latter case, keep only the common part
            if Current_Path'Last < Info.Path'Last then
               Set_Info_Path (Current_Path);
            end if;
         end;
      end;
   end Process_Identifier;


   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit (Scope : in Asis.Element) is
      pragma Unreferenced (Scope);
      use Framework.Reports;
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Declarations, Asis.Elements;

      function Scope_Image (Elem : Asis.Element) return Wide_String is
      begin
         case Declaration_Kind (Elem) is
            when A_Procedure_Declaration | A_Procedure_Body_Declaration =>
               return "procedure " & Defining_Name_Image (Names (Elem)(1));
            when A_Function_Declaration | A_Function_Body_Declaration =>
               return "procedure " & Defining_Name_Image (Names (Elem)(1));
            when A_Package_Declaration | A_Package_Body_Declaration =>
               return "procedure " & Defining_Name_Image (Names (Elem)(1));
            when others =>
               -- Including Not_A_Declaration
               null;
         end case;

         case Statement_Kind (Elem) is
            when A_Block_Statement =>
               return "block";
            when others =>
               null;
         end case;

         return "scope";
      end Scope_Image;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Local_Declarations.Reset (Current_Scope_Only);
      while Local_Declarations.Data_Available loop
         declare
            Info : constant Declaration_Info := Local_Declarations.Current_Data;
         begin
            if Info.Path = null then
               Report (Rule_Id,
                       To_Wide_String (Rule_Label),
                       Rule_Type,
                       Get_Location (Info.Elem),
                       Defining_Name_Image (Info.Elem) & " is not used");
            else
               Report (Rule_Id,
                       To_Wide_String (Rule_Label),
                       Rule_Type,
                       Get_Location (Info.Elem),
                       "declaration of " & Defining_Name_Image (Info.Elem)
                       & " can be moved inside " & Scope_Image (Info.Path (Info.Path'Last))
                       & " at " & Image (Get_Location (Info.Path (Info.Path'Last))));
            end if;
         end;

         Local_Declarations.Next;
      end loop;

   end Process_Scope_Exit;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Reduceable_Scope;
