----------------------------------------------------------------------
--  Rules.Usage - Package body                                      --
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
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Usage is
   use Framework, Ada.Strings.Wide_Unbounded;

   -- Algorithm:
   --
   -- For each identifier or defining name encountered, we keep in the Usage table how
   -- it has been used (if declared in a package spec).
   -- For generics, we keep the same information also in Cumulated_Usage.
   -- For the usage of objects declared in instances, the actual usage is the union of
   -- both tables.
   --
   -- Logically, we gather information about *all* elements, and print the report at the
   -- end of the run. Of course, this means that we store information about all entities
   -- encountered, which is quite costly (we do not release space when the scope where
   -- the entity is declared is left, for example).
   -- Currently, we optimize only the case where all usages are "from_spec". In this case,
   -- we do not store not "from_spec" entities.
   -- More optimizations could be added later.
   --
   -- For exceptions, tasks, and protected we use the same mechanisms, using:
   -- Exceptions: handled <=> read, raised  <=> written.
   -- Tasks     : called  <=> read, aborted <=> written.
   -- Protected : called  <=> read
   -- Appropriate renamings are provided for these equivalences



   -- In the following type, K_Declared is not visible to users of the rule, since
   -- an enitity is always declared! However, it does not necessarily mean that the
   -- declaration is processed (if the corresponding unit is not processed).
   -- Objects whose declaration is not processed are not reported.
   type Usage_Kind is (K_Declared, K_From_Spec, K_Initialized, K_Read, K_Written);
   subtype Rule_Usage_Kind is Usage_Kind range Usage_Kind'Succ (K_Declared) .. Usage_Kind'Last;
   K_Handled : Rule_Usage_Kind renames K_Read;
   K_Raised  : Rule_Usage_Kind renames K_Written;
   K_Called  : Rule_Usage_Kind renames K_Read;
   K_Aborted : Rule_Usage_Kind renames K_Written;

   type Entity_Kind is (K_Object, K_All, K_Variable, K_Constant, K_Exception, K_Task, K_Protected, K_Other);
   -- K_Object and K_all are there just for the user, not used passed parameter analysis. Must stay first in type.
   -- K_Other is used internally, not visible to the user. Must stay last in type.
   subtype User_Entity_Kind     is Entity_Kind range Entity_Kind'First        .. Entity_Kind'Pred (K_Other);
   subtype True_Entity_Kind     is Entity_Kind range Entity_Kind'Succ (K_All) .. Entity_Kind'Pred (K_Other);
   subtype Extended_Entity_Kind is Entity_Kind range Entity_Kind'Succ (K_All) .. Entity_Kind'Last;

   package Entity_Flags_Utilities is new Framework.Language.Flag_Utilities (User_Entity_Kind, Prefix => "K_");
   use Entity_Flags_Utilities;

   type Usage_Value is array (Usage_Kind) of Boolean;
   type Usage_Record is
      record
         Declaration : Asis.Expression;
         Entity      : True_Entity_Kind;
         Usage       : Usage_Value;
      end record;

   package Usage_Map is new Binary_Map (Unbounded_Wide_String,
                                        Usage_Record,
                                        Ada.Strings.Wide_Unbounded."<",
                                        Ada.Strings.Wide_Unbounded.">");
   Usage           : Usage_Map.Map;
   Cumulated_Usage : Usage_Map.Map; -- For Objects declared in generics

   -- Rule Applicability
   type Label_Table is array (Rule_Types) of Unbounded_Wide_String;

   type Rule_Info is
      record
         Used_Types : Rule_Types_Set;
         Labels     : Label_Table;
      end record;

   -- Indices of following table correspond to True_Entity_Kind, K_From_Spec, K_Initialized, K_Read, K_Written
   -- in that order
   Rule_Table : array (True_Entity_Kind, Boolean, Boolean, Boolean, Boolean) of Rule_Info
     := (others => (others => (others => (others => (others =>
                                            (Used_Types => (others => False),
                                             Labels     => (others => Null_Unbounded_Wide_String))
                                                    )))));
   All_From_Spec : Boolean := True;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): variable | object {, [not] from_spec | initialized | read | written}");
      User_Message ("  or        : constant          {, [not] from_spec | read}");
      User_Message ("  or        : exception         {, [not] from_spec | raised | handled}");
      User_Message ("  or        : task              {, [not] from_spec | called | aborted}");
      User_Message ("  or        : protected         {, [not] from_spec | called}");
      User_Message ("  or        : all               [, [not] from_spec]");
      User_Message ("Control usage of entities declared in package specifications");
      User_Message ("(possibly restricted to those that match the specified properties)");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Framework.Language;

      type General_Condition is (None, Found, Not_Found, Both);
      subtype Condition is General_Condition range Found .. Not_Found;
      type Rule_Usage_Value is array (Rule_Usage_Kind) of General_Condition;

      Value_Mask  : Rule_Usage_Value := (others => None);
      Entity      : User_Entity_Kind;
      Usage_Param : Rule_Usage_Kind;
      Has_Not     : Boolean;
      Wide_Label  : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Label);

      procedure Update_Rule_Table (Kind : True_Entity_Kind; Usages : Rule_Usage_Value) is
         -- None is the same as Both for the initialization of Rule_Table
      begin
         for S in Condition loop
            if Usages (K_From_Spec) not in Condition or else Usages (K_From_Spec) = S then
               for I in Condition loop
                  if Usages (K_Initialized) not in Condition or else Usages (K_Initialized) = I then
                     for R in Condition loop
                        if Usages (K_Read) not in Condition or else Usages (K_Read) = R then
                           for W in Condition loop
                              if Usages (K_Written) not in Condition or else Usages (K_Written) = W then
                                 if Rule_Table (Kind,
                                                S = Found, I = Found, R = Found, W = Found).Used_Types (Rule_Type)
                                 then
                                    Parameter_Error ("This combination of values already specified for rule "
                                                       & Rule_Id);
                                 else
                                    Rule_Table (Kind, S = Found, I = Found, R = Found, W = Found).Used_Types (Rule_Type)
                                      := True;
                                    Rule_Table (Kind, S = Found, I = Found, R = Found, W = Found).Labels (Rule_Type)
                                      := Wide_Label;
                                 end if;
                              end if;
                           end loop;
                        end if;
                     end loop;
                  end if;
               end loop;
            end if;
         end loop;
      end Update_Rule_Table;

      Bad_KW        : constant Wide_String := "Unexpected keyword: ";
      Already_Given : constant Wide_String := "Parameter value already given";
   begin -- Add_Use
      if not Parameter_Exists then
         Parameter_Error ("Parameter required for rule " & Rule_Id);
      end if;

      Entity := Get_Flag_Parameter (Allow_Any => False);

      -- Note: the syntax is too complicated to use Flag_Utilities to parse the Usage_Kind
      while Parameter_Exists loop
         Has_Not := Get_Modifier ("NOT");
         declare
            use Utilities;
            To_Compare : constant Wide_String := To_Upper (Get_String_Parameter);
         begin
            if To_Compare = "FROM_SPEC" then
               Usage_Param := K_From_Spec;
            else
               case Entity is
                  when K_All =>
                     -- Only From_Spec allowed
                     Parameter_Error (Bad_KW & To_Compare);

                  when K_Variable | K_Object =>
                     if To_Compare = "READ" then
                        Usage_Param := K_Read;
                     elsif To_Compare = "WRITTEN" then
                        Usage_Param := K_Written;
                     elsif To_Compare = "INITIALIZED" then
                        Usage_Param := K_Initialized;
                     else
                        Parameter_Error (Bad_KW & To_Compare);
                     end if;

                  when K_Constant =>
                     if To_Compare = "READ" then
                        Usage_Param := K_Read;
                     else
                        Parameter_Error (Bad_KW & To_Compare);
                     end if;

                  when K_Exception =>
                     if To_Compare = "RAISED" then
                        Usage_Param := K_Raised;
                     elsif To_Compare = "HANDLED" then
                        Usage_Param := K_Handled;
                     else
                        Parameter_Error (Bad_KW & To_Compare);
                     end if;

                  when K_Task =>
                     if To_Compare = "CALLED" then
                        Usage_Param := K_Called;
                     elsif To_Compare = "ABORTED" then
                        Usage_Param := K_Aborted;
                     else
                        Parameter_Error (Bad_KW & To_Compare);
                     end if;

                  when K_Protected =>
                     if To_Compare = "CALLED" then
                        Usage_Param := K_Called;
                     else
                        Parameter_Error (Bad_KW & To_Compare);
                     end if;

               end case;
            end if;
         end;

         case Value_Mask (Usage_Param) is
            when None =>
               if Has_Not then
                  Value_Mask (Usage_Param) := Not_Found;
               else
                  Value_Mask (Usage_Param) := Found;
               end if;
            when Found =>
               if Has_Not then
                  Value_Mask (Usage_Param) := Both;
               else
                  Parameter_Error (Already_Given);
               end if;
            when Not_Found =>
               if Has_Not then
                  Parameter_Error (Already_Given);
               else
                  Value_Mask (Usage_Param) := Both;
               end if;
            when Both =>
               Parameter_Error (Already_Given);
         end case;
      end loop;

      case Entity is
         when K_All =>
            Update_Rule_Table (K_Variable,  Value_Mask);
            Update_Rule_Table (K_Constant,  Value_Mask);
            Update_Rule_Table (K_Exception, Value_Mask);
            Update_Rule_Table (K_Task,      Value_Mask);
            Update_Rule_Table (K_Protected, Value_Mask);
         when K_Object =>
            Update_Rule_Table (K_Variable, Value_Mask);
            Update_Rule_Table (K_Constant, Value_Mask);
         when others =>
            Update_Rule_Table (Entity, Value_Mask);
      end case;

      if Value_Mask (K_From_Spec) /= Found then
         All_From_Spec := False;
      end if;
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
            Rule_Used  := False;
            Rule_Table := (others =>
                             (others =>
                                (others =>
                                   (others =>
                                      (others => (Used_Types => (others => False),
                                                  Labels     => (others => Null_Unbounded_Wide_String))
                                   )))));
            Clear (Usage);
            All_From_Spec := True;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ------------
   -- Update --
   ------------

   procedure Update (Element : Asis.Element; Entity : True_Entity_Kind; Value : Usage_Value) is
      -- Precondition:
      -- Element is a Defining_Name of the Ultimate_Name of an identifier
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Usage_Map, Thick_Queries;

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
                                                Entity      => Entity,
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
                                                   Entity      => Entity,
                                                   Usage       => (others => False)));
         Entry_Value.Usage := Entry_Value.Usage or Value;
         Add (To => Cumulated_Usage, Key => Unbounded_Key, Value => Entry_Value);
      end if;
   end Update;


   --------------------------
   -- Is_From_Package_Spec --
   --------------------------

   function Is_From_Package_Spec (Element : in Asis.Declaration) return Boolean is
      use Asis, Asis.Elements, Thick_Queries;
      Enclosing_PU : constant Asis.Defining_Name := Enclosing_Program_Unit (Element);
   begin
      if Is_Nil (Enclosing_PU) then
         -- Element is a compilation unit
         return False;
      end if;

      case Declaration_Kind (Enclosing_Element (Enclosing_PU)) is
         when A_Package_Declaration | A_Generic_Package_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Is_From_Package_Spec;


   ----------------------
   -- Declaration_Form --
   ----------------------

   function Declaration_Form (Element : Asis.Declaration) return Extended_Entity_Kind is
      -- Expected element kind: A_Declaration
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      Temp : Asis.Element;
   begin
      if All_From_Spec and not Is_From_Package_Spec (Element) then
         -- Optimization:
         -- We check only elements from packages, no need to consider this one
         return K_Other;
      end if;

      case Declaration_Kind (Element) is
         when A_Variable_Declaration =>
            Temp := Object_Declaration_View (Element);
            if Definition_Kind (Temp) = A_Subtype_Indication then
               Temp := Subtype_Simple_Name (Temp);

               if Expression_Kind (Temp) = An_Attribute_Reference then
                  -- Type is T'Base or T'Class => not a task or protected type
                  return K_Variable;

               elsif Is_Type_Declaration_Kind (Corresponding_Name_Declaration (Temp),
                                               A_Task_Type_Declaration)
               then
                  return K_Task;

               elsif Is_Type_Declaration_Kind (Corresponding_Name_Declaration (Temp),
                                               A_Protected_Type_Declaration)
               then
                  return K_Protected;

               else
                  return K_Variable;
               end if;
            else
               return K_Variable;
            end if;

         when A_Constant_Declaration
           | A_Deferred_Constant_Declaration
           | A_Number_Declaration
           =>
            return K_Constant;

         when An_Exception_Declaration =>
            return K_Exception;

         when A_Single_Task_Declaration =>
            return K_Task;

         when A_Single_Protected_Declaration =>
            return K_Protected;

        when others =>
            return K_Other;
      end case;
   end Declaration_Form;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      use Asis.Elements, Usage_Map, Reports;
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

         -- Determine usage

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
                                                                  Entity      => Value.Entity,
                                                                  Usage       => (others => False))).Usage;
         elsif Is_Part_Of_Generic (Value.Declaration) then
            Origin     := From_Generic;

            -- Add usages from all instances
            True_Usage := Value.Usage or Fetch (From => Cumulated_Usage,
                                                Key  => Key,
                                                Default_Value => (Declaration => Value.Declaration,
                                                                  Entity      => Value.Entity,
                                                                  Usage       => (others => False))).Usage;

         else
            Origin     := Normal;
            True_Usage := Value.Usage;
         end if;

         if Rule_Table (Value.Entity,
                        True_Usage (K_From_Spec),
                        True_Usage (K_Initialized),
                        True_Usage (K_Read),
                        True_Usage (K_Written)).Used_Types = (Rule_Types => False)
         then
            -- This combination not checked
            return;
         end if;

         if not True_Usage (K_Declared) then
            -- Declaration was not in the processed units => ignore
            return;
         end if;

         -- Prepare message

         case Origin is
            when Normal =>
               Message := To_Unbounded_Wide_String ("(normal) ");
            when From_Instance =>
               Message := To_Unbounded_Wide_String ("(instance) ");
            when From_Generic =>
               Message := To_Unbounded_Wide_String ("(generic) ");
         end case;
         Append (Message, Full_Name_Image (Value.Declaration));

         case Value.Entity is
            when K_Constant =>
               Append (Message, ", constant");
               -- no need to tell that a constant is initialized and not written
               Usage_Message (True_Usage (K_Read), "read");
               if not True_Usage (K_Read)
                 and Origin /= From_Instance
               then
                  Append (Message," (can be removed)");
               end if;

            when K_Variable =>
               Append (Message, ", variable");
               Usage_Message (True_Usage (K_Initialized), "initialized");
               Usage_Message (True_Usage (K_Written), "written");
               Usage_Message (True_Usage (K_Read), "read");

               if not True_Usage (K_Read)
                 and not True_Usage (K_Written)
                 and Origin /= From_Instance
               then
                  Append (Message," (can be removed)");
               elsif True_Usage (K_Read)
                 and not (True_Usage (K_Initialized) or True_Usage (K_Written))
               then
                  Append (Message," (never given a value)");
               elsif not True_Usage (K_Written)
                 and True_Usage (K_Initialized)
                 and Origin /= From_Instance
               then
                  Append (Message," (can be declared constant)");
               end if;

            when K_Exception =>
               Append (Message, ", exception");
               Usage_Message (True_Usage (K_Raised), "raised");
               Usage_Message (True_Usage (K_Handled), "handled");

            when K_Task =>
               Append (Message, ", task");
               Usage_Message (True_Usage (K_Called), "called");
               Usage_Message (True_Usage (K_Aborted), "aborted");

            when K_Protected =>
               Append (Message, ", protected");
               Usage_Message (True_Usage (K_Called), "called");
         end case;

         -- Do actual report

         if Rule_Table (Value.Entity,
                        True_Usage(K_From_Spec),
                        True_Usage(K_Initialized),
                        True_Usage(K_Read),
                        True_Usage(K_Written)).Used_Types (Check)
         then
            Report (Rule_Id,
                    To_Wide_String (Rule_Table (Value.Entity,
                                                True_Usage(K_From_Spec),
                                                True_Usage(K_Initialized),
                                                True_Usage(K_Read),
                                                True_Usage(K_Written)).Labels (Check)),
                    Check,
                    Elem_Loc,
                    To_Wide_String (Message));

         elsif Rule_Table (Value.Entity,
                           True_Usage(K_From_Spec),
                           True_Usage(K_Initialized),
                           True_Usage(K_Read),
                           True_Usage(K_Written)).Used_Types (Search)
         then
            Report (Rule_Id,
                    To_Wide_String (Rule_Table (Value.Entity,
                                                True_Usage(K_From_Spec),
                                                True_Usage(K_Initialized),
                                                True_Usage(K_Read),
                                                True_Usage(K_Written)).Labels (Search)),
                    Search,
                    Elem_Loc,
                    To_Wide_String (Message));
         end if;

         if Rule_Table (Value.Entity,
                        True_Usage(K_From_Spec),
                        True_Usage(K_Initialized),
                        True_Usage(K_Read),
                        True_Usage(K_Written)).Used_Types (Count)
         then
            Report (Rule_Id,
                    To_Wide_String (Rule_Table (Value.Entity,
                                                True_Usage(K_From_Spec),
                                                True_Usage(K_Initialized),
                                                True_Usage(K_Read),
                                                True_Usage(K_Written)).Labels (Count)),
                    Count,
                    Elem_Loc,
                    "");
         end if;
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
   -- Process_Entity_Declaration --
   --------------------------------

   procedure Process_Entity_Declaration (Element : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Form (Element) is
         when K_Variable =>
            -- Element is A_Variable_Declaration here

            -- Analyze the type for the case where it is an access type, or
            -- an array of accesses, since these are initialized by the compiler.
            declare
               The_Names       : constant Asis.Defining_Name_List := Names (Element);
               Is_Initialized  : Boolean;
               Root_Definition : Asis.Definition;
            begin
               if Is_Nil (Initialization_Expression (Element)) then
                  Root_Definition := Object_Declaration_View (Element);
                  if Definition_Kind (Root_Definition) = A_Subtype_Indication then
                     declare
                        Name : Asis.Expression := Subtype_Simple_Name (Root_Definition);
                     begin
                        if Expression_Kind (Name) = An_Attribute_Reference then
                           -- 'Base is for a discrete type
                           -- 'Class is for a tagged type
                           -- Both are not special cases tested below, we can therefore
                           -- take the prefix as well for Root_Definition
                           Name := Prefix (Name);
                           if Expression_Kind (Name) = A_Selected_Component then
                              Name := Selector (Name);
                           end if;
                        end if;
                        Root_Definition := Type_Declaration_View (Corresponding_First_Subtype
                                                                    (Corresponding_Name_Declaration
                                                                       (Name)));
                     end;
                     if Type_Kind (Root_Definition)
                       in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
                     then
                        Root_Definition := Type_Declaration_View (A4G_Bugs.Corresponding_Root_Type (Root_Definition));
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
                                 Root_Definition := Type_Declaration_View (Corresponding_Name_Declaration
                                                                             (Subtype_Simple_Name
                                                                                (Component_Subtype_Indication
                                                                                   (Array_Component_Definition
                                                                                      (Root_Definition)))));
                                 exit when Type_Kind (Root_Definition)
                                   not in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition;
                              end loop;
                              Is_Initialized := Type_Kind (Root_Definition) = An_Access_Type_Definition;
                           when Not_A_Type_Definition =>
                              Failure ("Wrong definition", Root_Definition);
                           when others =>
                              Is_Initialized := False;
                        end case;
                     when A_Private_Type_Definition
                       | A_Tagged_Private_Type_Definition
                       | A_Private_Extension_Definition
                       | A_Formal_Type_Definition
                       | A_Task_Definition
                       | A_Protected_Definition
                       =>
                        Is_Initialized := False;
                     when others =>
                        Failure ("Unexpected definition", Root_Definition);
                  end case;

               else  -- not Is_Nil (Initialization_Expression (Element))
                  Is_Initialized := True;
               end if;

               for I in The_Names'Range loop
                  Update (The_Names (I),
                          K_Variable,
                          (K_Declared    => True,
                           K_From_Spec   => Is_From_Package_Spec (Element),
                           K_Initialized => Is_Initialized,
                           others => False));
               end loop;
            end;

         when K_Constant =>
            declare
               The_Names : constant Asis.Defining_Name_List := Names (Element);
            begin
               for I in The_Names'Range loop
                  Update (The_Names (I),
                          K_Constant,
                          (K_Declared | K_Initialized => True,
                           K_From_Spec                => Is_From_Package_Spec (Element),
                           others                     => False));
               end loop;
            end;

         when K_Exception =>
            declare
               The_Names : constant Asis.Defining_Name_List := Names (Element);
            begin
               for I in The_Names'Range loop
                  Update (The_Names (I),
                          K_Exception,
                          (K_Declared  => True,
                           K_From_Spec => Is_From_Package_Spec (Element),
                           others      => False));
               end loop;
            end;

         when K_Task =>
            declare
               The_Names : constant Asis.Defining_Name_List := Names (Element);
            begin
               for I in The_Names'Range loop
                  Update (The_Names (I),
                          K_Task,
                          (K_Declared  => True,
                           K_From_Spec => Is_From_Package_Spec (Element),
                           others      => False));
               end loop;
            end;

          when K_Protected =>
             declare
                The_Names : constant Asis.Defining_Name_List := Names (Element);
             begin
                for I in The_Names'Range loop
                   Update (The_Names (I),
                           K_Protected,
                           (K_Declared  => True,
                            K_From_Spec => Is_From_Package_Spec (Element),
                            others      => False));
                end loop;
             end;

        when K_Other =>
           null;
      end case;
   end Process_Entity_Declaration;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Name : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Reports, Thick_Queries, Utilities;

      Good_Name : Asis.Expression;
      Item      : Asis.Element;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- If name is an actual in-out of a generic instantiation, do nothing
      -- (actual usage will be found by analyzing the expanded template)
      Item := Enclosing_Element (Name);
      loop
         case Element_Kind (Item) is
            when An_Expression =>
               Item := Enclosing_Element (Item);
            when An_Association =>
               if Association_Kind (Item) = A_Generic_Association
                 and then Mode_Kind (Enclosing_Element (Formal_Name (Item))) = An_In_Out_Mode
               then
                  return;
               else
                  exit;
               end if;
            when others =>
               exit;
         end case;
      end loop;

      begin
         Good_Name := Ultimate_Name (Name);
      exception
         when Asis.Exceptions.ASIS_Failed =>
            -- Due to an ASIS-for-GNAT bug, Ultimate_Name fails if the parameter is a generic
            -- formal "in" parameter. However, if this parameter corresponds to a variable,
            -- it has already been marked as read at the level of the instantiation.
            -- Therefore, we can catch and ignore this case.
            A4G_Bugs.Trace_Bug ("Rules.Usage.Process_Identifier");
            return;
      end;

      if Is_Nil (Good_Name) then
         -- Target of a renaming not statically known
         if Is_Part_Of_Instance (Name) then
            -- Name may be an artificial name without a location
            -- Put the message at the location of the instantiation
            Uncheckable (Rule_Id,
                         False_Negative,
                         Get_Location (Ultimate_Enclosing_Instantiation (Name)),
                         "Name """ & Name_Image(Name) & """is dynamic renaming");
         else
            Uncheckable (Rule_Id,
                         False_Negative,
                         Get_Location (Name),
                         "Name is dynamic renaming");
         end if;
         return;
      elsif Expression_Kind (Good_Name) = An_Attribute_Reference then
         -- Renaming of an attribute: certainly not defined in a package
         return;
      end if;

      case Declaration_Form (Corresponding_Name_Declaration (Good_Name)) is
         when K_Variable =>
            case Expression_Usage_Kind (Name) is
               when Untouched =>
                  null;
               when Read =>
                  Update (Good_Name, K_Variable, Value => (K_Read => True, others => False));
               when Write =>
                  Update (Good_Name, K_Variable, Value => (K_Written => True, others => False));
               when Read_Write =>
                  Update (Good_Name, K_Variable, Value => (K_Read | K_Written => True, others => False));
            end case;

         when K_Constant =>
            if Expression_Usage_Kind (Name) /= Untouched then
               Update (Good_Name, K_Constant, Value => (K_Read => True, others => False));
            end if;

         when K_Exception =>
            declare
               Enclosing : Asis.Element := Enclosing_Element (Name);
            begin
               while Expression_Kind (Enclosing) = A_Selected_Component loop
                  Enclosing := Enclosing_Element (Enclosing);
               end loop;

               if Expression_Kind (Enclosing) = An_Attribute_Reference
                 and then A4G_Bugs.Attribute_Kind (Enclosing) = An_Identity_Attribute
               then
                  Enclosing := Enclosing_Element (Enclosing);
                  if Element_Kind (Enclosing) = An_Association then
                     Enclosing := Enclosing_Element (Enclosing);
                     if Statement_Kind (Enclosing) = A_Procedure_Call_Statement then
                        Enclosing := Called_Name (Enclosing);
                        -- Beware: E'Identity can be a parameter to a call to an entry family member
                        -- in which case Called_Name can return an indexed component.
                        -- Not interesting for us, but it would crash Full_Name_Image
                        if Expression_Kind (Enclosing) /= An_Indexed_Component
                          and then To_Upper (Full_Name_Image (Enclosing)) = "ADA.EXCEPTIONS.RAISE_EXCEPTION"
                        then
                           Update (Good_Name, K_Exception, Value => (K_Raised => True, others => False));
                        end if;
                     end if;
                  end if;

               elsif Statement_Kind (Enclosing) = A_Raise_Statement then
                  Update (Good_Name, K_Exception, Value => (K_Raised => True, others => False));

               else
                  -- What else could it be than an exception handler?
                  Update (Good_Name, K_Exception, Value => (K_Handled => True, others => False));
               end if;
            end;

         when K_Task =>
            declare
               Enclosing : Asis.Element := Enclosing_Element (Name);
            begin
               while Expression_Kind (Enclosing) = A_Selected_Component loop
                  Enclosing := Enclosing_Element (Enclosing);
               end loop;

               if Expression_Kind (Enclosing) = An_Attribute_Reference
                 and then A4G_Bugs.Attribute_Kind (Enclosing) = An_Identity_Attribute
               then
                  Enclosing := Enclosing_Element (Enclosing);
                  if Element_Kind (Enclosing) = An_Association then
                     Enclosing := Enclosing_Element (Enclosing);
                     if Statement_Kind (Enclosing) = A_Procedure_Call_Statement then
                        Enclosing := Called_Name (Enclosing);
                        -- Beware: T'Identity can be a parameter to a call to an entry family member
                        -- in which case Called_Name can return an indexed component.
                        -- Not interesting for us, but it would crash Full_Name_Image
                        if Expression_Kind (Enclosing) /= An_Indexed_Component
                          and then To_Upper (Full_Name_Image (Enclosing)) = "ADA.TASK_IDENTIFICATION.ABORT_TASK"
                        then
                           Update (Good_Name, K_Task, Value => (K_Aborted => True, others => False));
                        end if;
                     end if;
                  end if;

               else
                  case Statement_Kind (Enclosing) is
                     when An_Abort_Statement =>
                        Update (Good_Name, K_Task, Value => (K_Aborted => True, others => False));
                     when An_Entry_Call_Statement =>
                        Update (Good_Name, K_Task, Value => (K_Called => True, others => False));
                     when others =>
                        null;
                  end case;
               end if;
            end;

         when K_Protected =>
            declare
               Enclosing : Asis.Element := Enclosing_Element (Name);
            begin
               while Expression_Kind (Enclosing) = A_Selected_Component loop
                  Enclosing := Enclosing_Element (Enclosing);
               end loop;

               case Statement_Kind (Enclosing) is
                  when Not_A_Statement =>
                     if Expression_Kind (Enclosing) = A_Function_Call then
                        Update (Good_Name, K_Protected, Value => (K_Called => True, others => False));
                     end if;
                  when An_Entry_Call_Statement | A_Procedure_Call_Statement =>
                     Update (Good_Name, K_Protected, Value => (K_Called => True, others => False));
                  when others =>
                     null;
               end case;
            end;
         when K_Other =>
            null;
      end case;
   end Process_Identifier;


   -----------------------------------
   -- Process_Package_Instantiation --
   -----------------------------------

   procedure Process_Package_Instantiation (Instantiation : Asis.Declaration) is
      use Asis, Asis.Elements, Asis.Iterator, Asis.Declarations;

      The_Info    : Null_State;
      The_Control : Traverse_Control := Continue;

      -- Process Object declarations from the instantiated package specification
      procedure Pre_Procedure_Decl (Element : in     Asis.Element;
                                    Control : in out Asis.Traverse_Control;
                                    State   : in out Null_State);
      -- Process identifiers from the instantiated package (specification and body)
      procedure Pre_Procedure_Ident (Element : in     Asis.Element;
                                     Control : in out Asis.Traverse_Control;
                                     State   : in out Null_State);

      procedure Traverse_Decl is new Asis.Iterator.Traverse_Element
        (Null_State, Pre_Procedure_Decl, Null_State_Procedure);
      procedure Traverse_Ident is new Asis.Iterator.Traverse_Element
        (Null_State, Pre_Procedure_Ident, Null_State_Procedure);

      procedure Pre_Procedure_Decl (Element : in     Asis.Element;
                                    Control : in out Asis.Traverse_Control;
                                    State   : in out Null_State)
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
                  Process_Entity_Declaration (Element);
               end if;
            when others =>
               null;
         end case;
      end Pre_Procedure_Decl;

      procedure Pre_Procedure_Ident (Element : in     Asis.Element;
                                     Control : in out Asis.Traverse_Control;
                                     State   : in out Null_State)
      is
         use Asis.Expressions;
      begin
         case Element_Kind (Element) is
            when An_Expression =>
               case Expression_Kind (Element) is
                  when An_Identifier
                    | An_Operator_Symbol
                    | An_Enumeration_Literal
                    =>
                     Process_Identifier (Element);

                  when An_Attribute_Reference =>
                     -- Don't traverse the selector, therefore we traverse manually the prefix
                     Traverse_Ident (Prefix (Element), Control, State);
                     Control := Abandon_Children;
                  when others =>
                     null;
               end case;

            when A_Pragma =>
               -- Nothing interesting here, and pragma elements are dangerous...
               Control := Abandon_Children;

            when others =>
               null;
         end case;
      end Pre_Procedure_Ident;

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Instantiated_Spec : constant Asis.Declaration := Corresponding_Declaration (Instantiation);
         Instantiated_Body : constant Asis.Declaration := Corresponding_Body        (Instantiation);
      begin
         Traverse_Decl  (Instantiated_Spec, The_Control, The_Info);
         Traverse_Ident (Instantiated_Spec, The_Control, The_Info);

         if not Is_Nil (Instantiated_Body) then
            Traverse_Ident (Instantiated_Body, The_Control, The_Info);
         end if;
      end;
   end Process_Package_Instantiation;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help     => Help'Access,
                                              Add_Use  => Add_Use'Access,
                                              Command  => Command'Access,
                                              Finalize => Finalize'Access);
end Rules.Usage;
