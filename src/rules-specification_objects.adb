----------------------------------------------------------------------
--  Rules.Specification_Objects - Package body                      --
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
  Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
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
  Framework.Reports,
  Utilities;

package body Rules.Specification_Objects is
   use Framework;

   -- Algorithm:
   --
   -- For each identifier or defining name encountered, we keep in the Usage table how
   -- it has been used (if declared in a package spec).
   -- For generics, we keep the same information also in Cumulated_Usage.
   -- For the usage of objects declared in instances, the actual usage is the union of
   -- both tables.
   --
   -- The tricky part is the diagnosis of "write" usage (usage as the LHS of an assignment, or
   -- as an [in] out parmeter). When we are at an identifier, it would be very difficult to determine
   -- that we are in such a context. Rather, when we encounter an assignment or a call, we traverse
   -- the LHS (or in [out] parameters) and keep in the Target_Variables table the variables encountered.
   -- When these variables will be later discovered by the recursive descent, we'll check in the table,
   -- to discover their usage.
   -- Entries in the Target_Variables table are freed in the post-procedure for assignment or calls.



   -- In the following type, K_Declared is not visible to users of the rule, since
   -- an object is always declared! However, it does not necessary mean that the
   -- declaration is processed (if the corresponding unit is not processed).
   -- Objects whose declaration is not processed are not reported.
   type Usage_Kind is (K_Declared, K_Constant, K_Initialized, K_Read, K_Written);
   subtype Rule_Usage_Kind is Usage_Kind range K_Constant .. K_Written;

   type Usage_Value is array (Usage_Kind) of Boolean;
   type Rule_Usage_Value is array (Rule_Usage_Kind) of Boolean;
   type Usage_Record is
      record
         Declaration : Asis.Expression;
         Usage       : Usage_Value;
      end record;

   package Usage_Map is new Binary_Map (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
                                        Usage_Record,
                                        Ada.Strings.Wide_Unbounded."<",
                                        Ada.Strings.Wide_Unbounded.">");
   Usage           : Usage_Map.Map;
   Cumulated_Usage : Usage_Map.Map; -- For Objects declared in generics

   type Target_Record is
      record
         Element : Asis.Element;
         Usage   : Usage_Value;
      end record;
   Target_Variables : array (1 .. Max_Parameters) of Target_Record;
   TV_Length        : Natural := 0;   -- Nb of used entries in Target_Variables

   -- Rule Applicability
   type Rule_Info (Used : Boolean := False) is
      record
         case Used is
            when True =>
               Rule_Type  : Rule_Types;
               Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
            when False =>
               null;
         end case;
      end record;

   -- Indices of following table correspond to K_Constant, K_Initialized, K_Read, K_Written in that order
   Rule_Table : array (Boolean, Boolean, Boolean, Boolean) of Rule_Info
     := (others => (others => (others => (others => (Used => False)))));

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   ------------
   -- Update --
   ------------

   procedure Update (Element : Asis.Element; Value : Usage_Value) is
      -- Precondition:
      -- Element is a Defining_Name of the Ultimate_Name of an identifier

      use Ada.Strings.Wide_Unbounded, Usage_Map, Asis, Asis.Elements, Asis.Expressions;
      use Asis.Declarations, Thick_Queries;
      Unbounded_Key : Unbounded_Wide_String;
      Entry_Value   : Usage_Record;
      Default_Decl  : Asis.Defining_Name;
   begin
      if Element_Kind (Element) = A_Defining_Name then
         Default_Decl := Element;
      else
         Default_Decl := Corresponding_Name_Definition (Element);
      end if;

      Unbounded_Key := To_Unbounded_Wide_String (Full_Name_Image (Default_Decl));
      Entry_Value   := Fetch (From          => Usage,
                              Key           => Unbounded_Key,
                              Default_Value => (Declaration => Default_Decl,
                                                Usage       => (others => False)));
      Entry_Value.Usage := Entry_Value.Usage or Value;
      Add (To => Usage, Key => Unbounded_Key, Value => Entry_Value);

      -- For Objects that are part of an instance, accumulate usage
      -- for the corresponding generic element
      if Is_Part_Of_Instance (Default_Decl) then
         Unbounded_Key := To_Unbounded_Wide_String (Full_Name_Image
                                                    (Corresponding_Generic_Element (Default_Decl)));
         Entry_Value   := Fetch (From          => Cumulated_Usage,
                                 Key           => Unbounded_Key,
                                 Default_Value => (Declaration => Default_Decl,
                                                   Usage       => (others => False)));
         Entry_Value.Usage := Entry_Value.Usage or Value;
         Add (To => Cumulated_Usage, Key => Unbounded_Key, Value => Entry_Value);
      end if;
   end Update;

   ----------------------
   -- Declaration_Form --
   ----------------------

   type Declaration_Location is (Var_From_Spec, Const_From_Spec, Not_From_Spec);

   function Declaration_Form (Element : Asis.Declaration) return Declaration_Location is
      -- Expected element: A_Declaration
      use Asis, Asis.Elements, Thick_Queries;
   begin
      case Declaration_Kind (Element) is
         when A_Variable_Declaration =>
            case Declaration_Kind (Enclosing_Element (Enclosing_Program_Unit (Element))) is
               when A_Package_Declaration
                 | A_Generic_Package_Declaration
                 =>
                  return Var_From_Spec;
               when others =>
                  return Not_From_Spec;
            end case;

         when A_Constant_Declaration
           | A_Deferred_Constant_Declaration
           | A_Number_Declaration
           =>
            case Declaration_Kind (Enclosing_Element (Enclosing_Program_Unit (Element))) is
               when A_Package_Declaration | A_Generic_Package_Declaration =>
                  return Const_From_Spec;
               when others =>
                  return Not_From_Spec;
            end case;

         when others =>
            return Not_From_Spec;
      end case;
   end Declaration_Form;

   -------------------
   -- Mark_Variable --
   -------------------

   procedure Mark_Variable (Element : Asis.Expression; Usage : Usage_Value) is
      use Asis, Asis.Elements, Asis.Expressions, Utilities, Thick_Queries;
      Target_Element : Asis.Element := Element;
   begin

      -- Get rid of view conversion
      if Expression_Kind (Target_Element) = A_Type_Conversion then
         Target_Element := Converted_Or_Qualified_Expression (Target_Element);
      end if;

      -- In the general case, the LHS of an assignment will look like:
      -- Pack1.Pack2.OBJ.Field1(4).Field2 := ...
      -- We are interested in retrieving the object (i.e. the OBJ part)
      -- Note that the object can be a constant, but only if it is of an access type, and then
      -- it must be followed by an implicit or explicit dereference
      -- Note also that there can be type conversions (or qualifications) along the way...
      loop
         case Expression_Kind (Target_Element) is
            when A_Selected_Component =>
               if Expression_Type_Kind (Prefix (Target_Element)) = An_Access_Type_Definition then
                  -- This selected component is an implicit dereference
                  -- => the variable is not modified
                  return;
               elsif Declaration_Kind (Corresponding_Name_Declaration (Selector (Target_Element)))
                 = A_Variable_Declaration
               then
                  -- Object found
                 Target_Element := Selector (Target_Element);
                  exit;
               else
                  -- Must be a record field
                  Target_Element := Prefix (Target_Element);
               end if;

            when An_Indexed_Component | A_Slice =>
               Target_Element := Prefix (Target_Element);

            when An_Explicit_Dereference
              | A_Function_Call =>
               -- Explicit dereference => the object is not modified
               -- Function call => it must return an access type
               return;

            when An_Identifier =>
               exit;

            when A_Type_Conversion
              | A_Qualified_Expression
              =>
               -- The variable is inside the conversion/qualification
               Target_Element := Converted_Or_Qualified_Expression (Target_Element);

            when others =>
               Failure ("Unexpected expression in Mark_Variable", Target_Element);
         end case;
      end loop;

      TV_Length := TV_Length +1;
      Target_Variables (TV_Length) := (Target_Element, Usage);
   end Mark_Variable;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): [not] constant | initialized | read | written (optional, default=all)");
      User_Message ("Control usage of objects declared in package specifications");
      User_Message ("(possibly restricted to those that match the specified properties)");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded, Framework.Language;

      function Get_Usage_Parameter is new Get_Flag_Parameter (Flags     => Rule_Usage_Kind,
                                                              Allow_Any => False,
                                                              Prefix    => "K_");
      Mask        : Rule_Usage_Value := (others => False);
      Usage_Param : Rule_Usage_Kind;
      Usage_Truth : Rule_Usage_Value;
      Truth       : Boolean;
   begin
      while Parameter_Exists loop
         Truth       := not Get_Modifier ("NOT");
         Usage_Param := Get_Usage_Parameter;
         if Mask (Usage_Param) then
            Parameter_Error ("This value already specified in parameters");
         else
            Mask (Usage_Param) := True;
         end if;
         Usage_Truth (Usage_Param) := Truth;
      end loop;

      for C in Boolean loop
         if not Mask (K_Constant) or else Usage_Truth (K_Constant) = C then
            for I in Boolean loop
               if not Mask (K_Initialized) or else Usage_Truth (K_Initialized) = I then
                  for R in Boolean loop
                     if not Mask (K_Read) or else Usage_Truth (K_Read) = R then
                        for W in Boolean loop
                           if not Mask (K_Written) or else Usage_Truth (K_Written) = W then
                              if Rule_Table (C, I, R, W).Used then
                                 Parameter_Error ("This combination of values already specified for rule "
                                                  & Rule_ID);
                              else
                                 Rule_Table (C, I, R, W) := (True,
                                                             Rule_Type,
                                                             To_Unbounded_Wide_String (Label));
                              end if;
                           end if;
                        end loop;
                     end if;
                  end loop;
               end if;
            end loop;
         end if;
      end loop;

      Rule_Used := True;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Usage_Map, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Clear (Usage);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      use Asis.Elements, Usage_Map, Ada.Strings.Wide_Unbounded, Reports;
      procedure Report_One (Key : Unbounded_Wide_String; Value : in out Usage_Record) is
         use Asis.Declarations, Thick_Queries;

         type Origin_Kind is (Normal, From_Instance, From_Generic);

         Message      : Unbounded_Wide_String;
         True_Usage   : Usage_Value;
         Elem_Loc     : Location;
         Origin       : Origin_Kind;

         procedure Usage_Message (Used : Boolean; Usage_Mess : Wide_String)  is
         begin
            if Used then
               Append (Message, ", " & Usage_Mess);
            else
               Append (Message, ", not " & Usage_Mess);
            end if;
         end Usage_Message;

      begin
         Elem_Loc :=  Get_Location (Value.Declaration);

         if Is_Part_Of_Instance (Value.Declaration) then
            Origin := From_Instance;

            -- Let the location refer to the instantiation
            Elem_Loc := Get_Location (Ultimate_Enclosing_Instantiation (Value.Declaration));

            -- Add usages from the generic itself
            True_Usage := Value.Usage or Fetch (From => Usage,
                                                Key  => To_Unbounded_Wide_String (Full_Name_Image
                                                                                  (Corresponding_Generic_Element
                                                                                   (Value.Declaration))),
                                                Default_Value => (Declaration => Value.Declaration,
                                                                  Usage       => (others => False))).Usage;
         elsif Is_Part_Of_Generic (Value.Declaration) then
            Origin     := From_Generic;
            True_Usage := Value.Usage or Fetch (From => Cumulated_Usage,
                                                Key  => Key,
                                                Default_Value => (Declaration => Value.Declaration,
                                                                  Usage       => (others => False))).Usage;

         else
            Origin     := Normal;
            True_Usage := Value.Usage;
         end if;

         if not Rule_Table (True_Usage(K_Constant),
                            True_Usage(K_Initialized),
                            True_Usage(K_Read),
                            True_Usage(K_Written)).Used
         then
            return;
         end if;

         if not True_Usage (K_Declared) then
            -- Declaration was not in the processed units => ignore
            return;
         end if;

         case Origin is
            when Normal =>
               Message := To_Unbounded_Wide_String ("(normal) ");
            when From_Instance =>
               Message := To_Unbounded_Wide_String ("(instance) ");
            when From_Generic =>
               Message := To_Unbounded_Wide_String ("(generic) ");
         end case;
         Append (Message,
                 Defining_Name_Image (Enclosing_Program_Unit (Value.Declaration))
                 & '.' & Defining_Name_Image (Value.Declaration));

         if True_Usage (K_Constant) then
            Append (Message, ", constant");
            -- no need to tell that a constant is not written
         else
            Usage_Message (True_Usage (K_Initialized), "initialized");
            Usage_Message (True_Usage (K_Written), "written");
         end if;

         Usage_Message (True_Usage (K_Read), "read");

         -- Extra information
         if not True_Usage (K_Read)
           and not True_Usage (K_Written)
           and Origin /= From_Instance
         then
            Append (Message," (can be removed)");

         elsif True_Usage (K_Read)
           and not (True_Usage (K_Initialized) or True_Usage (K_Written))
         then
            Append (Message," (never given a value)");

         elsif True_Usage (K_Initialized)
           and not True_Usage (K_Written)
           and not True_Usage (K_Constant)   -- if it is already a constant...
           and Origin /= From_Instance
         then
            Append (Message," (can be declared constant)");
         end if;

         Report (Rule_Id,
                 To_Wide_String (Rule_Table (True_Usage(K_Constant),
                                             True_Usage(K_Initialized),
                                             True_Usage(K_Read),
                                             True_Usage(K_Written)).Rule_Label),
                 Rule_Table (True_Usage(K_Constant),
                             True_Usage(K_Initialized),
                             True_Usage(K_Read),
                             True_Usage(K_Written)).Rule_Type,
                 Elem_Loc,
                 To_Wide_String (Message));
      end Report_One;

      procedure Report_All is new Iterate (Report_One);
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Report_All (Usage);
   end Finalize;

   --------------------------------
   -- Process_Object_Declaration --
   --------------------------------

   procedure Process_Object_Declaration (Element : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Utilities;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Form (Element) is
         when Var_From_Spec =>
            -- Element is A_Variable_Declaration here
            declare
               The_Names       : constant Asis.Defining_Name_List := Names (Element);
               Is_Initialized  : Boolean;
               Root_Definition : Asis.Definition := Object_Declaration_View (Element);
               ST_Name         : Asis.Expression;
            begin
               if Definition_Kind (Root_Definition) = A_Subtype_Indication then
                  ST_Name := Asis.Definitions.Subtype_Mark (Root_Definition);
                  if Expression_Kind (ST_Name) = A_Selected_Component then
                     ST_Name := Selector (ST_Name);
                  end if;
                  Root_Definition := Type_Declaration_View (Corresponding_First_Subtype
                                                            (Corresponding_Name_Declaration
                                                             (ST_Name)));
                  if Type_Kind (Root_Definition)
                    in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
                  then
                     Root_Definition := Type_Declaration_View (Corresponding_Root_Type (Root_Definition));
                  end if;
               end if;

               case Definition_Kind (Root_Definition) is
                  when A_Type_Definition =>
                     case Type_Kind (Root_Definition) is
                        when An_Access_Type_Definition =>
                           -- Access types are always initialized
                           Is_Initialized := True;
                        when An_Unconstrained_Array_Definition
                          | A_Constrained_Array_Definition
                          =>
                           -- So are arrays of accesses (to any depth)
                           loop
                              ST_Name := Asis.Definitions.Subtype_Mark (Component_Subtype_Indication
                                                                        (Array_Component_Definition
                                                                         (Root_Definition)));
                              if Expression_Kind (ST_Name) = A_Selected_Component then
                                 ST_Name := Selector (ST_Name);
                              end if;
                              Root_Definition := Type_Declaration_View (Corresponding_Name_Declaration (ST_Name));
                              exit when Type_Kind (Root_Definition)
                                        not in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition;
                           end loop;
                           Is_Initialized := Type_Kind (Root_Definition) = An_Access_Type_Definition;
                        when Not_A_Type_Definition =>
                           Failure ("Wrong definition", Root_Definition);
                        when others =>
                           Is_Initialized := not Is_Nil (Initialization_Expression (Element));
                     end case;
                  when A_Private_Type_Definition
                    | A_Tagged_Private_Type_Definition
                    | A_Private_Extension_Definition
                    | A_Formal_Type_Definition
                    =>
                     Is_Initialized := not Is_Nil (Initialization_Expression (Element));
                  when A_Task_Definition
                    | A_Protected_Definition
                    =>
                     Is_Initialized := False;
                  when others =>
                     Failure ("Unexpected definition", Root_Definition);
              end case;

               for I in The_Names'Range loop
                  Update (The_Names (I),
                          (K_Declared => True, K_Initialized => Is_Initialized, others => False));
               end loop;
            end;

         when Const_From_Spec =>
            declare
               The_Names : constant Asis.Defining_Name_List := Names (Element);
            begin
               for I in The_Names'Range loop
                  Update (The_Names (I),
                          (K_Declared | K_Constant | K_Initialized => True, others => False));
               end loop;
            end;

         when Not_From_Spec =>
            null;
      end case;
   end Process_Object_Declaration;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Name : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions, Thick_Queries;
      Good_Name : Asis.Expression;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Good_Name := Ultimate_Name (Name);
      if Is_Nil (Good_Name) then
         -- Target of a renaming not statically known
         return;
      elsif Expression_Kind (Good_Name) = An_Attribute_Reference then
         -- Renaming of an attribute: certainly not defined in a package
         return;
      end if;

      case Declaration_Form (Corresponding_Name_Declaration (Good_Name)) is
         when Var_From_Spec =>
            for I in Positive range 1 .. TV_Length loop
               if Is_Equal (Name, Target_Variables (I).Element) then
                  Update (Good_Name, Value => Target_Variables (I).Usage);
                  return;
               end if;
            end loop;

            -- Not a target of assignment or [in] out parameter
            Update (Good_Name, Value => (K_Read => True, others => False));

         when Const_From_Spec =>
            Update (Good_Name, Value => (K_Read => True, others => False));

         when Not_From_Spec =>
            null;
      end case;
   end Process_Identifier;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call (Call : in Asis.Statement) is
      use Asis, Asis.Statements, Asis.Elements, Asis.Expressions, Thick_Queries;
      Actuals : constant Asis.Association_List := Call_Statement_Parameters (Call);
      Mode    : Mode_Kinds;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Dispatching_Call (Call) then
         -- Improvement needed here, but it's quite difficult
         return;
      end if;

      for I in Actuals'Range loop
         Mode := Mode_Kind (Enclosing_Element (Formal_Name (Call, I)));
         if Mode = An_Out_Mode or Mode = An_In_Out_Mode then
            Mark_Variable (Actual_Parameter (Actuals (I)),
                           Usage => (K_Written => True, K_Read => Mode = An_In_Out_Mode, others => False));
         end if;
      end loop;
   end Process_Call;

   ----------------------------------
   -- Process_Assignment_Statement --
   ----------------------------------

   procedure Process_Assignment_Statement (Statement : in Asis.Statement) is
      use Asis.Statements;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Mark_Variable (Assignment_Variable_Name (Statement),
                     Usage => (K_Written => True, others => False));
   end Process_Assignment_Statement;

   -----------------------------------
   -- Process_Package_Instantiation --
   -----------------------------------

   procedure Process_Package_Instantiation (Instantiation : Asis.Declaration) is
      use Asis, Asis.Elements, Asis.Iterator, Asis.Declarations;

      type Info is null record;
      The_Info    : Info;
      The_Control : Traverse_Control := Continue;

      -- Process Object declarations from the instantiated package
      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Asis.Traverse_Control;
                               State   : in out Info)
      is
         pragma Unreferenced (Control, State);
      begin
         case Declaration_Kind (Element) is
            when A_Variable_Declaration
              | A_Constant_Declaration
              | A_Deferred_Constant_Declaration
              | A_Number_Declaration
              =>
               -- Do not consider pseudo declarations of constants or variables that
               -- correspond, in the expanded template, to the formals of the generic.
               if Declaration_Kind (Enclosing_Element
                                    (Corresponding_Generic_Element
                                     (Names (Element)(1))))
                 /= A_Formal_Object_Declaration
               then
                  Process_Object_Declaration (Element);
               end if;
            when others =>
               null;
         end case;
      end Pre_Procedure;

      procedure Post_Procedure (Element : in     Asis.Element;
                                Control : in out Asis.Traverse_Control;
                                State   : in out Info)
      is
         pragma Unreferenced (Element, Control, State);
      begin
         null;
      end Post_Procedure;

      procedure Traverse is new Asis.Iterator.Traverse_Element
        (Info, Pre_Procedure, Post_Procedure);

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Traverse (Corresponding_Declaration (Instantiation), The_Control, The_Info);
   end Process_Package_Instantiation;

   ------------------
   -- Post_Process --
   ------------------

   procedure Post_Process is
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      TV_Length := 0;
   end Post_Process;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help     => Help'Access,
                                     Add_Use  => Add_Use'Access,
                                     Command  => Command'Access,
                                     Finalize => Finalize'Access);
end Rules.Specification_Objects;
