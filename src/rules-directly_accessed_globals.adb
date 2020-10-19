----------------------------------------------------------------------
--  Rules.Directly_Accessed_Globals - Package body                  --
--                                                                  --
--  This software is (c) CSEE and Adalog 2004-2006.                 --
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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Binary_Map,
  Framework.Queries,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Directly_Accessed_Globals is
   use Framework, Framework.Control_Manager;

   type Filters is (F_Plain, F_Accept, F_Protected);
   package Filter_Flags_Utilities is new Framework.Language.Flag_Utilities (Filters, "F_");
   use Filter_Flags_Utilities;

   Rule_Used    : Boolean := False;
   Save_Used    : Boolean;
   Rule_Context : Basic_Rule_Context;
   Flags        : array (Filters) of Boolean := (others => False);

   type Variable_Info is
      record
         Owner_Pack : Asis.Element;
         Var_Loc    : Locations.Location;
         Read_Proc  : Asis.Defining_Name;
         Write_Proc : Asis.Defining_Name;
      end record;

   package Variables_Map is new Binary_Map
     (Key_Type   => Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
      Value_Type => Variable_Info,
      "<"        => Ada.Strings.Wide_Unbounded."<",
      ">"        => Ada.Strings.Wide_Unbounded.">");

   Global_Variables : Variables_Map.Map;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control global package variables accessed by other than dedicated subprograms");
      User_Message;
      Help_On_Flags (Header => "Parameter(s):", Footer => "(optional)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Utilities;
      F : Filters;
   begin
      if Rule_Used then
         Parameter_Error (Rule_Id, "this rule can be specified only once");
      end if;

      if Parameter_Exists then
         while Parameter_Exists loop
            F := Get_Flag_Parameter (Allow_Any => False);
            if Flags (F) then
               Parameter_Error (Rule_Id, Image (F, Lower_Case) & " already given");
            end if;
            Flags (F) := True;
         end loop;
      else
         Flags := (others => True);
      end if;

      Rule_Context := Basic.New_Context (Ctl_Kind, Ctl_Label);
      Rule_Used    := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Flags     := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ----------------------------------
   -- Process_Variable_Declaration --
   ----------------------------------

   procedure Process_Variable_Declaration (Decl : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Framework.Locations, Framework.Queries, Variables_Map;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Declaration_Kind (Enclosing_Element (Decl)) /= A_Package_Body_Declaration then
         return;
      end if;

      -- Note that since we are in a package /body/, the declaration is always processed
      -- before any use.
      for N : Asis.Name of  Names (Decl) loop
         Add (Global_Variables,
              To_Key (N),
              Variable_Info'(Owner_Pack             => Enclosing_Element (Decl),
                             Var_Loc                => Get_Location (N),
                             Read_Proc | Write_Proc => Nil_Element));
      end loop;
   end Process_Variable_Declaration;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Name : in Asis.Expression) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Ada.Strings.Wide_Unbounded;
      use Framework.Queries, Framework.Locations, Framework.Reports, Thick_Queries, Utilities, Variables_Map;
      Good_Name : Asis.Expression;
      Name_Decl : Asis.Declaration;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Expression_Kind (Name) /= An_Identifier then
         -- An_Operator_Symbol f.e., cannot be a variable
         return;
      end if;

      Good_Name := Ultimate_Name (Name, No_Component => True);
      if Is_Nil (Good_Name) then
         -- Dynamic renaming...
         Uncheckable (Rule_Id,
                      False_Negative,
                      Get_Location (Name),
                      "Name is dynamic renaming");
         return;
      end if;

      if Expression_Kind (Good_Name) = An_Attribute_Reference then
         -- can happen when Name is a renaming of an attribute
         -- certainly not a variable
         return;
      end if;

      Name_Decl := Corresponding_Name_Declaration (Good_Name);
      if Is_Nil (Name_Decl) then
         -- Some predefined stuff...
         return;
      end if;

      case Declaration_Kind (Name_Decl) is
         when A_Variable_Declaration
           | A_Single_Task_Declaration
           | A_Single_Protected_Declaration
           =>
            null;
         when others =>
            -- Not a variable
            return;
      end case;

      -- Here we have an acceptable variable

      declare
         Var_Name : constant Unbounded_Wide_String := To_Key (Good_Name);
         Var_Info : Variable_Info                  := Fetch (Global_Variables, Var_Name);

         Usage     : constant Expression_Usage_Kinds := Expression_Usage_Kind (Name);

         Unit_Name : constant Asis.Defining_Name     := Enclosing_Program_Unit (Name, Including_Accept => True);
         Unit_Decl : constant Asis.Declaration       := Enclosing_Element (Unit_Name);
         Unit_Kind : constant Declaration_Kinds      := Declaration_Kind (Unit_Decl) ;

         Encl_Unit_Decl : Asis.Element;
      begin
         if Usage = Untouched then
            -- Since we used Ultimate_Name, we won't be fooled by renamings.
            -- => we can allow them at any place
            return;
         end if;

         case Unit_Kind is
            when A_Procedure_Body_Declaration
              | A_Function_Body_Declaration
              | An_Entry_Declaration       -- Case of accept
              | An_Entry_Body_Declaration  -- Case of protected entry
              =>
               null;
            when others =>
               Report (Rule_Id,
                       Rule_Context,
                       Get_Location (Name),
                       "use of variable """ & Name_Image (Good_Name) & """ not from callable entity");
               return;
         end case;

         Encl_Unit_Decl := Enclosing_Element (Unit_Decl);
         if Element_Kind (Encl_Unit_Decl) = A_Definition then
            -- A_Task_Definition
            Encl_Unit_Decl := Enclosing_Element (Encl_Unit_Decl);
         end if;
         case Declaration_Kind (Encl_Unit_Decl) is
            when A_Single_Task_Declaration =>
               if not Flags (F_Accept) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """ & Name_Image (Good_Name) & """ from accept");
               elsif not Is_Equal (Corresponding_Body (Enclosing_Element (Encl_Unit_Decl)), Var_Info.Owner_Pack) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """ & Name_Image (Good_Name) & """ from nested task object");
               end if;

            when A_Task_Type_Declaration =>
               if Flags (F_Accept) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """ & Name_Image (Good_Name) & """ from accept of a task type");
               else
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """ & Name_Image (Good_Name) & """ from accept");
               end if;

            when A_Protected_Body_Declaration =>
               if not Flags (F_Protected) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """ & Name_Image (Good_Name)
                            & """ from subprogram of a protected type or object");
               elsif Declaration_Kind (Corresponding_Declaration (Encl_Unit_Decl))
                 /= A_Single_Protected_Declaration
               then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """
                          & Name_Image (Good_Name)
                          & """ from subprogram of a protected type");
               elsif not Is_Equal (Enclosing_Element (Encl_Unit_Decl), Var_Info.Owner_Pack) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """
                          & Name_Image (Good_Name)
                          & """ from nested protected object");
               end if;

            when others =>  -- Plain
               if not Flags (F_Plain) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """
                          & Name_Image (Good_Name)
                          & """ from a non-protected subprogram");
               elsif Declaration_Kind (Corresponding_Declaration (Unit_Decl)) in A_Generic_Declaration then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """ & Name_Image (Good_Name) & """ from generic subprogram");
               elsif not Is_Equal (Encl_Unit_Decl, Var_Info.Owner_Pack) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "use of variable """ & Name_Image (Good_Name) & """ from nested subprogram");
               end if;
         end case;

         case Usage is
            when Untouched =>
               Failure ("Untouched did not return");

            when Read =>
               if Is_Nil (Var_Info.Read_Proc) then
                  Var_Info.Read_Proc := Unit_Name;
                  Add (Global_Variables, Var_Name, Var_Info);
               elsif not Is_Equal (Var_Info.Read_Proc, Unit_Name) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "variable """ & Name_Image (Good_Name)
                            & """ is already read from " & Defining_Name_Image (Var_Info.Read_Proc)
                            & " at " & Image (Get_Location (Enclosing_Element (Var_Info.Read_Proc))));
               end if;

            when Write =>
               if Is_Nil (Var_Info.Write_Proc) then
                  Var_Info.Write_Proc := Unit_Name;
                  Add (Global_Variables, Var_Name, Var_Info);
               elsif not Is_Equal (Var_Info.Write_Proc, Unit_Name) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "variable """ & Name_Image (Good_Name)
                            & """ is already written from " & Defining_Name_Image (Var_Info.Write_Proc)
                            & " at " & Image (Get_Location (Enclosing_Element (Var_Info.Write_Proc))));
               end if;

            when Read_Write
               | Unknown     -- Consider Unknown as Read-Write, therefore creating false positives
               =>            -- That's better than false negatives!
               if Usage = Unknown then
                  Uncheckable (Rule_Id,
                               False_Positive,
                               Get_Location (Name),
                               "variable """ & Name_Image (Good_Name)
                               & """ used as parameter of dispatching call, treated as in-out");
               end if;

               if Is_Nil (Var_Info.Read_Proc) then
                  Var_Info.Read_Proc := Unit_Name;
                  Add (Global_Variables, Var_Name, Var_Info);
               elsif not Is_Equal (Var_Info.Read_Proc, Unit_Name) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "variable """ & Name_Image (Good_Name)
                            & """ is already read from " & Defining_Name_Image (Var_Info.Read_Proc)
                            & " at " & Image (Get_Location (Enclosing_Element (Var_Info.Read_Proc))));
               end if;

               if Is_Nil (Var_Info.Write_Proc) then
                  Var_Info.Write_Proc := Unit_Name;
                  Add (Global_Variables, Var_Name, Var_Info);
               elsif not Is_Equal (Var_Info.Write_Proc, Unit_Name) then
                  Report (Rule_Id,
                          Rule_Context,
                          Get_Location (Name),
                          "variable """ & Name_Image (Good_Name)
                            & """ is already written from " & Defining_Name_Image (Var_Info.Write_Proc)
                            & " at " & Image (Get_Location (Enclosing_Element (Var_Info.Write_Proc))));
               end if;
         end case;
      end;

   exception
      when Not_Present =>
         -- From Fetch: this is not a package variable
         return;
   end Process_Identifier;

   -------------------------------
   -- Post_Process_Package_Body --
   -------------------------------

   procedure Post_Process_Package_Body (Element : in Asis.Element) is
      use Variables_Map, Ada.Strings.Wide_Unbounded;

      procedure Check_One (Key : Unbounded_Wide_String; Var_Info : in out Variable_Info) is
         use Asis.Elements;
         use Framework.Reports, Utilities;
      begin
         if not Is_Equal (Var_Info.Owner_Pack, Element) then
            -- Possible with nested packages
            return;
         end if;

         if Is_Nil (Var_Info.Read_Proc) then
            Report (Rule_Id,
                    Rule_Context,
                    Var_Info.Var_Loc,
                    "variable """ &  To_Title (Strip_Profile (To_Wide_String (Key)))
                      & """ is not read from any subprogram");
         end if;

         if Is_Nil (Var_Info.Write_Proc) then
            Report (Rule_Id,
                    Rule_Context,
                    Var_Info.Var_Loc,
                    "variable """ &  To_Title (Strip_Profile (To_Wide_String (Key)))
                      & """ is not written from any subprogram");
         end if;

         -- Read_Proc/Write_Proc are the defining names of the procs
         -- The Enclosing_Element is the declaration, whose Enclosing_Element is the package body
         -- or protected body declaration.
         -- Since we already checked that the procs that are not protected are declared immediately
         -- within the same package as the variable, the bodies can be different only if the procs
         -- come from different protected objects or tasks.
         if (not Is_Nil (Var_Info.Read_Proc) and not Is_Nil (Var_Info.Write_Proc))
           and then not Is_Equal (Enclosing_Element (Enclosing_Element (Var_Info.Read_Proc)),
                                  Enclosing_Element (Enclosing_Element (Var_Info.Write_Proc)))
         then
            Report (Rule_Id,
                    Rule_Context,
                    Var_Info.Var_Loc,
                    "variable """ &  To_Title (Strip_Profile (To_Wide_String (Key)))
                      & """ is read and written from different protected objects or tasks");
         end if;

         raise Delete_Current;
      end Check_One;

      procedure Check_All is new Variables_Map.Iterate (Check_One);

   begin  -- Post_Process_Package_Body
      Check_All (Global_Variables);
   end Post_Process_Package_Body;

begin  -- Rules.Directly_Accessed_Globals
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Directly_Accessed_Globals;
