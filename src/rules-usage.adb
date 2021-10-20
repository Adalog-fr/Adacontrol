----------------------------------------------------------------------
--  Rules.Usage - Package body                                      --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2021.           --
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
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- AdaControl
with
   Framework.Language,
   Framework.Queries,
   Framework.Reports.Fixes;
pragma Elaborate (Framework.Language);

package body Rules.Usage is
   use Framework, Ada.Strings.Wide_Unbounded;

   -- Algorithm:
   --
   -- For each identifier or defining name encountered, we keep in the Usage table how
   -- it has been used.
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
   -- For subprograms, exceptions, tasks, and protected we use the same mechanisms, using:
   -- Subprograms: called       <=> read, accessed <=> written.
   -- Exceptions : handled      <=> read, raised   <=> written.
   -- Tasks      : called       <=> read, aborted  <=> written.
   -- Protected  : called       <=> read
   -- Generic    : instantiated <=> read
   -- Appropriate renamings are provided for these equivalences
   --
   -- Special case for From_Task_Guard:
   -- Usage from the guard itself and the matching accept statement is not reported.
   -- To that effect, when an identifier is encountered in a A_Select_Path or an An_Or_Path:
   --    - If it is part of the guard, it is pushed (with the path) on a stack of active select
   --      statements (since they can be nested). It is marked as From_Task_Guard, but not as read
   --    - If it is part of the accept statement, the stack is scanned to see if the identifier is used
   --      in the guard. If yes, it is marked as From_Task_Guard, but not as read/written
   -- The stack is freed as the post-processing of the path

   -- In the following type, K_Declared is not visible to users of the rule, since
   -- an entity is always declared! However, it does not necessarily mean that the
   -- declaration is processed (if the corresponding unit is not processed).
   -- Objects whose declaration is not processed are not reported.
   -- K_From_Spec is not a real usage kind, just a short_hand for (K_From_Visible or K_From_Private)
   --    must stay last in type.
   -- Note that "check usage (<entity>, not from_spec, ...)" is translated
   --      to "check usage (<entity>, not from_visible, not from_spec, ...)" ("and" semantics),
   --      while "check usage (<entity>, from_spec, ...)" is translated to
   --      "check usage (<entity>, from_visible, ...);check usage (<entity>, from_private, ...)" ("or" semantics)
   type Usage_Kind is (K_Declared,
                       K_From_Visible, K_From_Private, K_From_Task_Guard,
                       K_Initialized,  K_Read,         K_Written,
                       K_From_Spec);
   subtype User_Usage_Kind is Usage_Kind range Usage_Kind'Succ (K_Declared) .. Usage_Kind'Last;
   subtype Rule_Usage_Kind is Usage_Kind range Usage_Kind'First             .. Usage_Kind'Pred (K_From_Spec);
   K_Used         : Rule_Usage_Kind renames K_Read;
   K_Called       : Rule_Usage_Kind renames K_Read;
   K_Accessed     : Rule_Usage_Kind renames K_Written;
   K_Handled      : Rule_Usage_Kind renames K_Read;
   K_Raised       : Rule_Usage_Kind renames K_Written;
   K_Aborted      : Rule_Usage_Kind renames K_Written;
   K_Instantiated : Rule_Usage_Kind renames K_Read;

   type Entity_Kind is (K_Object,       K_All,
                        K_Variable,     K_Constant,      K_Type,             K_Procedure, K_Function,
                        K_In_Parameter, K_Out_Parameter, K_In_Out_Parameter,
                        K_Exception,    K_Task,          K_Protected,        K_Generic,
                        K_Other);
   -- K_Object and K_all are there just for the user, not used passed parameter analysis. Must stay first in type.
   -- K_Other is used internally, not visible to the user. Must stay last in type.
   subtype Subrules             is Entity_Kind range Entity_Kind'First        .. Entity_Kind'Pred (K_Other);
   subtype True_Entity_Kind     is Entity_Kind range Entity_Kind'Succ (K_All) .. Entity_Kind'Pred (K_Other);
   subtype Extended_Entity_Kind is Entity_Kind range True_Entity_Kind'First   .. Entity_Kind'Last;

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "K_");

   type Usage_Value is array (Rule_Usage_Kind) of Boolean;

   type Origin_Kind is (Normal, From_Instance, From_Generic);
   type Usage_Record is
      record
         Declaration      : Asis.Expression;
         Origin           : Origin_Kind;
         Decl_Location    : Locations.Location;
         Entity           : True_Entity_Kind;
         Usage            : Usage_Value;
      end record;

   package Usage_Map is new Binary_Map (Unbounded_Wide_String, Usage_Record);
   Own_Usage       : Usage_Map.Map;
   Cumulated_Usage : Usage_Map.Map; -- For Objects declared in generics

   type Guard_Record is
      record
         Guard_Elem : Asis.Name;
         Guard_Path : Asis.Path;
      end record;

   package Select_Path_Stack is new Linear_Queue (Guard_Record);
   Guard_Elements : Select_Path_Stack.Queue;

   type Package_Origins is (From_Visible, From_Private, Not_From_Spec);

   -- Rule Applicability
   type Label_Table is array (Control_Kinds) of Unbounded_Wide_String;

   type Rule_Info is
      record
         Used_Controls : Control_Kinds_Set;
         Labels        : Label_Table;
      end record;

   -- Indices of following table correspond to:
   --    True_Entity_Kind, K_From_Visible, K_From_Private, K_Initialized, K_Read, K_Written
   -- in that order
   Rule_Table : array (True_Entity_Kind, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) of Rule_Info
     := (others => (others => (others => (others => (others => (others => (others =>
                                            (Used_Controls => (others => False),
                                             Labels        => (others => Null_Unbounded_Wide_String))
                                                    )))))));
   All_From_Spec      : Boolean := True;
   From_Guard_Checked : Boolean := False;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control usage of various entities");
      User_Message ("(possibly restricted to those that match the specified location and properties)");
      User_Message;
      User_Message ("location ::= from_visible | from_private | from_spec | from_task_guard");
      User_Message ("Parameter(s): variable | object | <param> {, [not] <location> | initialized | read | written}");
      User_Message ("  or        : constant                    {, [not] <location> | read}");
      User_Message ("  or        : type                        {, [not] <location> | used}");
      User_Message ("  or        : procedure | function        {, [not] <location> | called}");
      User_Message ("  or        : exception                   {, [not] <location> | raised | handled}");
      User_Message ("  or        : task                        {, [not] <location> | called | aborted}");
      User_Message ("  or        : protected                   {, [not] <location> | called}");
      User_Message ("  or        : generic                     {, [not] <location> | instantiated}");
      User_Message ("  or        : all                         [, [not] <location>]");
      User_Message ("<param> ::= in_parameter | out_parameter | in_out_parameter");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   Bad_KW        : constant Wide_String := "unexpected keyword: ";
   Already_Given : constant Wide_String := "parameter value already given";

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flags_Utilities;

      -- Note: Not_Found means "not" has been found
      -- Order is important
      type General_Condition is (None, Not_Found, Found, Both);
      subtype Condition is General_Condition range Not_Found .. Found;
      type Rule_Usage_Value is array (Rule_Usage_Kind) of General_Condition;

      Value_Mask  : Rule_Usage_Value := (others => None);
      Value_Mask2 : Rule_Usage_Value;
      Subrule     : Subrules;
      Usage_Param : User_Usage_Kind;
      Has_Not     : Boolean;
      Wide_Label  : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Ctl_Label);

      Positive_From_Spec : Boolean := False;

      procedure Update_Mask_Entry (Cond : in out General_Condition; Inverted : Boolean) is
      begin
         case Cond is
            when None =>
               if Inverted then
                  Cond := Not_Found;
               else
                  Cond := Found;
               end if;
            when Found =>
               if Inverted then
                  Cond := Both;
               else
                  Parameter_Error (Rule_Id, Already_Given);
               end if;
            when Not_Found =>
               if Inverted then
                  Parameter_Error (Rule_Id, Already_Given);
               else
                  Cond := Both;
               end if;
            when Both =>
               Parameter_Error (Rule_Id, Already_Given);
         end case;
      end Update_Mask_Entry;

      procedure Update_Rule_Table (Kind : True_Entity_Kind; Usages : Rule_Usage_Value) is
         -- None is the same as Both for the initialization of Rule_Table
      begin
         for V in Condition loop
            if Usages (K_From_Visible) not in Condition or else Usages (K_From_Visible) = V then
               for P in Condition loop
                  if Usages (K_From_Private) not in Condition or else Usages (K_From_Private) = P then
                     for T in Condition loop
                        if Usages (K_From_Task_Guard) not in Condition or else Usages (K_From_Task_Guard) = T then
                           for I in Condition loop
                              if Usages (K_Initialized) not in Condition or else Usages (K_Initialized) = I then
                                 for R in Condition loop
                                    if Usages (K_Read) not in Condition or else Usages (K_Read) = R then
                                       for W in Condition loop
                                          if Usages (K_Written) not in Condition or else Usages (K_Written) = W then
                                             if Rule_Table (Kind,
                                                            V = Found, P = Found, T = Found,
                                                            I = Found, R = Found, W = Found)
                                               .Used_Controls (Ctl_Kind)
                                             then
                                                Parameter_Error (Rule_Id,
                                                                 "This combination of values already specified");
                                             else
                                                Rule_Table (Kind,
                                                            V = Found, P = Found, T = Found,
                                                            I = Found, R = Found, W = Found)
                                                  .Used_Controls (Ctl_Kind) := True;
                                                Rule_Table (Kind,
                                                            V = Found, P = Found, T = Found,
                                                            I = Found, R = Found, W = Found)
                                                  .Labels (Ctl_Kind) := Wide_Label;
                                             end if;
                                          end if;
                                       end loop;
                                    end if;
                                 end loop;
                              end if;
                           end loop;
                        end if;
                     end loop;
                  end if;
               end loop;
            end if;
         end loop;
      end Update_Rule_Table;

   begin -- Add_Control
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Parameter required");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);

      -- Note: the syntax is too complicated to use Flag_Utilities to parse the Usage_Kind
      while Parameter_Exists loop
         Has_Not := Get_Modifier ("NOT");
         declare
            To_Compare : constant Wide_String := Get_Name_Parameter;
         begin
            if To_Compare = "FROM_VISIBLE" then
               Usage_Param := K_From_Visible;
               if Has_Not then
                  All_From_Spec := False;
               end if;
            elsif To_Compare = "FROM_PRIVATE" then
               Usage_Param := K_From_Private;
               if Has_Not then
                  All_From_Spec := False;
               end if;
            elsif To_Compare = "FROM_SPEC" then
               Usage_Param := K_From_Spec;
               if Has_Not then
                  All_From_Spec := False;
               end if;
            elsif To_Compare = "FROM_TASK_GUARD" then
               Usage_Param        := K_From_Task_Guard;
               All_From_Spec      := False;
               From_Guard_Checked := True;
            else
               case Subrule is
                  when K_All =>
                     -- Only From_Spec, From_Visible, From_Private allowed
                     Parameter_Error (Rule_Id, Bad_KW & To_Compare);

                  when K_Variable | K_Object | K_In_Parameter | K_Out_Parameter | K_In_Out_Parameter =>
                     if To_Compare = "READ" then
                        Usage_Param := K_Read;
                     elsif To_Compare = "WRITTEN" then
                        Usage_Param := K_Written;
                     elsif To_Compare = "INITIALIZED" then
                        Usage_Param := K_Initialized;
                     else
                        Parameter_Error (Rule_Id, Bad_KW & To_Compare);
                     end if;

                  when K_Constant =>
                     if To_Compare = "READ" then
                        Usage_Param := K_Read;
                     else
                        Parameter_Error (Rule_Id, Bad_KW & To_Compare);
                     end if;

                  when K_Type =>
                     if To_Compare = "USED" then
                        Usage_Param := K_Used;
                     else
                        Parameter_Error (Rule_Id, Bad_KW & To_Compare);
                     end if;

                  when K_Procedure | K_Function =>
                     if To_Compare = "CALLED" then
                        Usage_Param := K_Called;
                     elsif To_Compare = "ACCESSED" then
                        Usage_Param := K_Accessed;
                     else
                        Parameter_Error (Rule_Id, Bad_KW & To_Compare);
                     end if;

                  when K_Exception =>
                     if To_Compare = "RAISED" then
                        Usage_Param := K_Raised;
                     elsif To_Compare = "HANDLED" then
                        Usage_Param := K_Handled;
                     else
                        Parameter_Error (Rule_Id, Bad_KW & To_Compare);
                     end if;

                  when K_Task =>
                     if To_Compare = "CALLED" then
                        Usage_Param := K_Called;
                     elsif To_Compare = "ABORTED" then
                        Usage_Param := K_Aborted;
                     else
                        Parameter_Error (Rule_Id, Bad_KW & To_Compare);
                     end if;

                  when K_Protected =>
                     if To_Compare = "CALLED" then
                        Usage_Param := K_Called;
                     else
                        Parameter_Error (Rule_Id, Bad_KW & To_Compare);
                     end if;

                  when K_Generic =>
                     if To_Compare = "INSTANTIATED" then
                        Usage_Param := K_Instantiated;
                     else
                        Parameter_Error (Rule_Id, Bad_KW & To_Compare);
                     end if;

               end case;
            end if;
         end;

         if Usage_Param = K_From_Spec then
            if Has_Not then
               Update_Mask_Entry (Value_Mask (K_From_Visible), Inverted => True);
               Update_Mask_Entry (Value_Mask (K_From_Private), Inverted => True);
            else
               Positive_From_Spec := True; -- will handle that case later
            end if;
         else
            Update_Mask_Entry (Value_Mask (Usage_Param), Has_Not);
         end if;
      end loop;

      if Value_Mask (K_From_Visible) = None
        and Value_Mask (K_From_Private) = None
        and not Positive_From_Spec
      then
         -- no positive place specified (negative have been handled already)
         All_From_Spec := False;
      end if;

      -- Ensure that From_Visible and From_Private are exclusive
      if Value_Mask (K_From_Visible) = Found and Value_Mask (K_From_Private) = Found then
         Parameter_Error (Rule_Id, "entity cannot be both in visible and private part");
      end if;

      -- From_Visible => not From_Private, From_Private => not From_Visible
      if Value_Mask (K_From_Visible) = Found and Value_Mask (K_From_Private) = None then
         Update_Mask_Entry (Value_Mask (K_From_Private), Inverted => True);
      end if;
      if Value_Mask (K_From_Private) = Found and Value_Mask (K_From_Visible) = None then
         Update_Mask_Entry (Value_Mask (K_From_Visible), Inverted => True);
      end if;


      if Positive_From_Spec then
         Value_Mask2 := Value_Mask;
         Update_Mask_Entry (Value_Mask  (K_From_Visible), Inverted => False);
         Update_Mask_Entry (Value_Mask  (K_From_Private), Inverted => True);
         Update_Mask_Entry (Value_Mask2 (K_From_Private), Inverted => False);
         Update_Mask_Entry (Value_Mask2 (K_From_Visible), Inverted => True);
      end if;

      case Subrule is
         when K_All =>
            for E in True_Entity_Kind loop
               Update_Rule_Table (E, Value_Mask);
               if Positive_From_Spec then
                  Update_Rule_Table (E, Value_Mask2);
               end if;
            end loop;
         when K_Object =>
            Update_Rule_Table (K_Variable, Value_Mask);
            Update_Rule_Table (K_Constant, Value_Mask);
            if Positive_From_Spec then
               Update_Rule_Table (K_Variable, Value_Mask2);
               Update_Rule_Table (K_Constant, Value_Mask2);
            end if;
         when others =>
            Update_Rule_Table (Subrule, Value_Mask);
            if Positive_From_Spec then
               Update_Rule_Table (Subrule, Value_Mask2);
            end if;
      end case;

      Rule_Used := True;
   end Add_Control;

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
                                      (others =>
                                         (others =>
                                            (others => (Used_Controls => (others => False),
                                                        Labels        => (others => Null_Unbounded_Wide_String))
                                   )))))));
            Clear (Own_Usage);
            Clear (Cumulated_Usage);
            All_From_Spec      := True;
            From_Guard_Checked := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   --------------------
   -- Package_Origin --
   --------------------

   function Package_Origin (Element : in Asis.Element) return Package_Origins is
      use Asis, Asis.Declarations, Asis.Elements;
      use Thick_Queries, Utilities;
      Name              : Asis.Defining_Name;
      Enclosing_PU_Name : Asis.Defining_Name;
      Enclosing_PU      : Asis.Declaration;
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            Name := Names (Element) (1);
         when A_Defining_Name =>
            Name := Element;
         when others =>
            Failure ("Package_Origin not on defining_name or declaration", Element);
      end case;

      if Declaration_Kind (Enclosing_Element (Enclosing_Element (Name))) in A_Formal_Declaration then
         -- Name is a name of a formal package f.e., not in private nor visible,
         -- and this case would run afoul of the rest of the algorithm
         return Not_From_Spec;
      end if;

      if Is_Part_Of_Instance (Name) then
         begin
            -- We are analyzing something from an expanded generic => no source is available,
            -- and Is_Part_Of does not work (as specified). We must therefore use the corresponding
            -- generic element.
            if Defining_Name_Kind (Name) = A_Defining_Expanded_Name then
               Name := Defining_Selector (Name);
            end if;
            Name := Corresponding_Generic_Element (Name);
         exception
            when Asis.Exceptions.Asis_Inappropriate_Element =>
               -- In some hard to reproduce cases, it seems that formal declarations are not classified as such, which
               -- in turn crashes Corresponding_Generic_Element.
               -- TBSL: this is really a work-around
               return Not_From_Spec;
         end;
      end if;

      Enclosing_PU_Name := Enclosing_Program_Unit (Name);
      if Is_Nil (Enclosing_PU_Name) then
         -- Element is a compilation unit
         return Not_From_Spec;
      end if;
      Enclosing_PU := Enclosing_Element (Enclosing_PU_Name);

      case Declaration_Kind (Enclosing_PU) is
         when A_Package_Declaration | A_Generic_Package_Declaration =>
            if Is_Part_Of (Name, Private_Part_Declarative_Items (Enclosing_PU)) then
               return From_Private;
            else
               return From_Visible;
            end if;
         when others =>
            return Not_From_Spec;
      end case;
   end Package_Origin;


   ------------
   -- Update --
   ------------

   procedure Update (Element : Asis.Element; Entity : True_Entity_Kind; Value : Usage_Value) is
      -- Precondition:
      -- Element is a Defining_Name, or the Ultimate_Name of an identifier
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Queries, Thick_Queries, Usage_Map;

      Unbounded_Key : Unbounded_Wide_String;
      Entry_Value   : Usage_Record;
      Default_Decl  : Asis.Defining_Name;
      Generic_Decl  : Asis.Defining_Name;
   begin
      if Element_Kind (Element) = A_Defining_Name then
         Default_Decl := Element;
      else
         Default_Decl := Corresponding_Name_Definition (Element);
      end if;

      Unbounded_Key := To_Key (Default_Decl);
      Entry_Value   := Fetch (From          => Own_Usage,
                              Key           => Unbounded_Key,
                              Default_Value => (Declaration      => Default_Decl,
                                                Origin           => Normal,
                                                Decl_Location    => Get_Location (Default_Decl),
                                                Entity           => Entity,
                                                Usage            => (others => False)));
      Entry_Value.Usage := Entry_Value.Usage or Value;
      -- Take a note of the origin now to avoid horrible tree swapping during the call to Finalize
      -- Mantis 0000010
      if Is_Part_Of_Instance (Default_Decl) then
         Entry_Value.Origin := From_Instance;
         -- Let the location refer to the instantiation
         Entry_Value.Decl_Location := Get_Location (Ultimate_Enclosing_Instantiation (Default_Decl));
      elsif Is_Part_Of_Generic (Default_Decl) then
         Entry_Value.Origin := From_Generic;
      end if;
      Add (To => Own_Usage, Key => Unbounded_Key, Value => Entry_Value);

      -- For Objects that are part of a package specification of an instance, accumulate usage
      -- from the outside for the corresponding generic element.
      if Entry_Value.Origin = From_Instance and then Package_Origin (Default_Decl) = From_Visible then
         Generic_Decl := Corresponding_Generic_Element (Default_Decl);
         -- Generic_Decl can be Nil for implicit elements inherited from a derivation in the generic,
         -- which have no corresponding generic elements.
         if not Is_Nil (Generic_Decl) then
            Unbounded_Key := To_Key (Generic_Decl);
            Entry_Value   := Fetch (From          => Cumulated_Usage,
                                    Key           => Unbounded_Key,
                                    Default_Value => (Declaration      => Default_Decl,
                                                      Origin           => From_Instance,
                                                      Decl_Location    => Entry_Value.Decl_Location,
                                                      Entity           => Entity,
                                                      Usage            => (others => False)));
            Entry_Value.Usage := Entry_Value.Usage or Value;
            Add (To => Cumulated_Usage, Key => Unbounded_Key, Value => Entry_Value);
         end if;
      end if;
   end Update;


   ----------------------
   -- Declaration_Form --
   ----------------------

   function Declaration_Form (Element : Asis.Element) return Extended_Entity_Kind is
      -- Expected element kind:
      --   A_Declaration
      --   An_Expression, expected Expression_Kind:
      --      An_Identifier
      --      An_Operator_Symbol
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Utilities;

      Temp : Asis.Element;
      Decl : Asis.Declaration;
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            Decl := Element;
         when An_Expression =>
            Decl := Corresponding_Name_Declaration (Element);
            case Element_Kind (Decl) is
               when Not_An_Element =>
                  -- We cannot handle things that have no declaration:
                  -- Predefined operator, formals of predefined operators...
                  return K_Other;
               when A_Declaration =>
                  -- All is well...
                  null;
               when A_Statement =>
                  if Statement_Kind (Decl) /= A_For_Loop_Statement then
                     -- Block or loop name...
                     return K_Other;
                  end if;

                  if Is_Equal (Corresponding_Name_Definition (Element), Statement_Identifier (Decl)) then
                     -- for loop name
                     return K_Other;
                  else
                     -- Must be the declaration of the loop control variable
                     return K_Constant;
                  end if;
               when others =>
                  Failure ("Strange declaration in Declaration_Form", Decl);
            end case;
         when others =>
            Failure ("Incorrect element in Declaration_Form", Element);
      end case;

      if All_From_Spec and then Package_Origin (Decl) = Not_From_Spec then
         -- Optimization:
         -- We check only elements from packages, no need to consider this one
         return K_Other;
      end if;

      case Declaration_Kind (Decl) is
         when A_Constant_Declaration
           | A_Deferred_Constant_Declaration
           | A_Number_Declaration
           =>
            return K_Constant;

         when A_Parameter_Specification =>
            case Mode_Kind (Decl) is
               when An_In_Mode | A_Default_In_Mode =>
                  return K_In_Parameter;
               when An_Out_Mode =>
                  return K_Out_Parameter;
               when An_In_Out_Mode =>
                  return K_In_Out_Parameter;
               when Not_A_Mode =>
                  Failure ("Usage: Not_A_Mode in parameter declaration", Decl);
            end case;

         when A_Variable_Declaration =>
            Temp := Object_Declaration_View (Decl);
            if Definition_Kind (Temp) /= A_Subtype_Indication then
               -- Must be A_Type_Definition -> A_Constrained_Array_Definition
               return K_Variable;
            end if;

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

         when A_Procedure_Body_Declaration
            | A_Procedure_Body_Stub
              =>
            if Is_Subunit (Decl) or not Is_Nil (Corresponding_Declaration (Decl)) then
               -- The check is performed on the specification or the stub, no need to repeat here
               return K_Other;
            end if;
            return K_Procedure;

         when A_Function_Body_Declaration
            | A_Function_Body_Stub
              =>
            if Is_Subunit (Decl) or not Is_Nil (Corresponding_Declaration (Decl)) then
               -- The check is performed on the specification or the stub, no need to repeat here
               return K_Other;
            end if;
            return K_Function;

         when A_Procedure_Declaration =>
            if Definition_Kind (Enclosing_Element (Decl)) = A_Protected_Definition then
               -- A protected procedure
               return K_Other;
            else
               return K_Procedure;
            end if;

         when A_Null_Procedure_Declaration =>
            return K_Procedure;

         when A_Function_Declaration
            | An_Expression_Function_Declaration   -- Ada 2012
            =>
            if Definition_Kind (Enclosing_Element (Decl)) = A_Protected_Definition then
               -- A protected function
               return K_Other;
            else
               return K_Function;
            end if;

         when A_Procedure_Instantiation =>
            return K_Procedure;

         when A_Function_Instantiation =>
            return K_Function;

         when An_Ordinary_Type_Declaration
            | A_Task_Type_Declaration
            | A_Protected_Type_Declaration
            | A_Subtype_Declaration
            =>
            return K_Type;

         when An_Exception_Declaration =>
            return K_Exception;

         when A_Single_Task_Declaration =>
            return K_Task;

         when A_Single_Protected_Declaration =>
            return K_Protected;

         when A_Generic_Declaration =>
            return K_Generic;

         when others =>
            return K_Other;
      end case;
   end Declaration_Form;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      use Usage_Map;

      procedure Report_One (Key : Unbounded_Wide_String; Value : in out Usage_Record) is
         use Asis.Declarations, Asis.Elements;
         use Framework.Queries, Framework.Locations, Framework.Reports, Utilities;

         Message      : Unbounded_Wide_String;
         True_Usage   : Usage_Value;
         Generic_Elem : Asis.Expression;
         Pseudo_Const : Boolean;

         procedure Usage_Message (Used : Boolean; Usage_Mess : Wide_String)  is
         begin
            if Used then
               Append (Message, ", " & Usage_Mess);
            else
               Append (Message, ", not " & Usage_Mess);
            end if;
         end Usage_Message;

         function Is_Pseudo_Const (Entity : Asis.Expression) return Boolean is
            use Thick_Queries;
         begin
            return (for some I of Discrete_Constraining_Lengths (Entity) => I = 0);
         exception
            when others =>
               -- Presumably, a storage_error (maybe changed into an Asis_Failed)
               -- due to too big an enumeration type (Wide_Character)
               -- better consider it is not a pseudo-const than crashing the program
               return False;
         end Is_Pseudo_Const;

      begin  -- Report_One
         -- Determine usage
         case Value.Origin is
            when From_Instance =>
               -- Add usages from the generic itself
               begin
                  Generic_Elem := Corresponding_Generic_Element (Value.Declaration);
                  -- Generic_Elem can be Nil for implicit elements inherited from a derivation in the generic,
                  -- which have no corresponding generic elements. But then, there are certainly no usage inside
                  -- the generic
               exception
                  when Asis.Exceptions.Asis_Inappropriate_Element =>
                     -- See comment in Package_Origin, the same can happen here
                     -- TBSL: this is really a work-around
                     Generic_Elem := Asis.Nil_Element;
               end;
               if Is_Nil (Generic_Elem) then
                  True_Usage := Value.Usage;
               else
                  True_Usage := Value.Usage
                                or Fetch (From          => Own_Usage,
                                          Key           => To_Key (Generic_Elem),
                                          Default_Value => (Declaration   => Value.Declaration,
                                                            Origin        => From_Instance,
                                                            Decl_Location => Null_Location, -- Since it's a dummy
                                                            Entity        => Value.Entity,
                                                            Usage         => (others => False))).Usage;
               end if;
            when From_Generic =>
               -- Add usages from all instances
               True_Usage := Value.Usage
                             or Fetch (From          => Cumulated_Usage,
                                       Key           => Key,
                                       Default_Value => (Declaration   => Value.Declaration,
                                                         Origin        => From_Generic,
                                                         Decl_Location => Null_Location,  -- Since it's a dummy
                                                         Entity        => Value.Entity,
                                                         Usage         => (others => False))).Usage;

            when Normal =>
               True_Usage := Value.Usage;
         end case;

         if Rule_Table (Value.Entity,
                        True_Usage (K_From_Visible),
                        True_Usage (K_From_Private),
                        True_Usage (K_From_Task_Guard),
                        True_Usage (K_Initialized),
                        True_Usage (K_Read),
                        True_Usage (K_Written)).Used_Controls = (Control_Kinds => False)
         then
            -- This combination not checked
            return;
         end if;

         if not True_Usage (K_Declared) then
            -- Declaration was not in the processed units => ignore
            return;
         end if;

         -- Prepare message

         case Value.Origin is
            when Normal =>
               Message := To_Unbounded_Wide_String ("(normal) ");
            when From_Instance =>
               Message := To_Unbounded_Wide_String ("(instance) ");
            when From_Generic =>
               Message := To_Unbounded_Wide_String ("(generic) ");
         end case;
         Append (Message, Strip_Profile (To_Wide_String (Key)));   -- Key is Full_Name_Image of defining name

         if True_Usage (K_From_Visible) then
            Append (Message, ", visible");
         elsif True_Usage (K_From_Private) then
            Append (Message, ", private");
         elsif True_Usage (K_From_Task_Guard) then
            Append (Message, ", in task guard");
         end if;

         -- Build usage message, and possibly give clever advices, unless From_Task_Guard is true,
         -- since usages from inside the guard and the accept statement are not accounted for.
         -- But remind the user of this.
         case Value.Entity is
            when K_Constant =>
               Append (Message, ", constant");
               -- no need to tell that a constant is initialized and not written
               Usage_Message (True_Usage (K_Read), "read");
               if True_Usage (K_From_Task_Guard) then
                  Append (Message, " (outside guard and corresponding accept)");
               elsif not True_Usage (K_Read)
                 and Value.Origin /= From_Instance
               then
                  Append (Message, " (can be removed)");
               end if;

            when K_Variable =>
               Pseudo_Const := Is_Pseudo_Const (Value.Declaration);
               Append (Message, ", variable");
               if not Pseudo_Const then
                  Usage_Message (True_Usage (K_Initialized), "initialized");
                  Usage_Message (True_Usage (K_Written), "written");
               end if;
               Usage_Message (True_Usage (K_Read), "read");

               if True_Usage (K_From_Task_Guard) then
                  Append (Message, " (outside guard and corresponding accept)");
               elsif not True_Usage (K_Read)
                 and not True_Usage (K_Written)
                 and Value.Origin /= From_Instance
               then
                  Append (Message, " (can be removed)");
               elsif Pseudo_Const then
                  Append (Message, " (pseudo constant)");
               elsif True_Usage (K_Read)
                 and not (True_Usage (K_Initialized) or True_Usage (K_Written))
               then
                  Append (Message, " (never given a value)");
               elsif not True_Usage (K_Written)
                 and True_Usage (K_Initialized)
                 and Value.Origin /= From_Instance
               then
                  Append (Message, " (can be declared constant)");
               end if;

            when K_In_Parameter =>
               Append (Message, ", in parameter");
               Usage_Message (True_Usage (K_Initialized), "initialized");
               Usage_Message (True_Usage (K_Read), "read");
               if True_Usage (K_From_Task_Guard) then
                  Append (Message, " (outside guard and corresponding accept)");
               elsif not True_Usage (K_Read)
                 and Value.Origin /= From_Instance
               then
                  Append (Message, " (unused)");
               end if;

            when K_Out_Parameter =>
               Append (Message, ", out parameter");
               Usage_Message (True_Usage (K_Written), "written");
               Usage_Message (True_Usage (K_Read), "read");
               if True_Usage (K_From_Task_Guard) then
                  Append (Message, " (outside guard and corresponding accept)");
               elsif not True_Usage (K_Written) then
                  Append (Message, " (unset out parameter)");
               end if;

            when K_In_Out_Parameter =>
               Append (Message, ", in out parameter");
               Usage_Message (True_Usage (K_Written), "written");
               Usage_Message (True_Usage (K_Read), "read");
               if True_Usage (K_From_Task_Guard) then
                  Append (Message, " (outside guard and corresponding accept)");
               elsif not True_Usage (K_Written) then
                  Append (Message, " (mode can be changed to in)");
               elsif not True_Usage (K_Read) then
                  Append (Message, " (mode can be changed to out)");
               end if;

            when K_Type =>
               Append (Message, ", (sub)type");
               Usage_Message (True_Usage (K_Used), "used");

            when K_Procedure =>
               Append (Message, ", procedure");
               Usage_Message (True_Usage (K_Called), "called");
               Usage_Message (True_Usage (K_Accessed), "accessed");

            when K_Function =>
               Append (Message, ", function");
               Usage_Message (True_Usage (K_Called), "called");
               Usage_Message (True_Usage (K_Accessed), "accessed");
               if True_Usage (K_From_Task_Guard) then
                  Append (Message, " (outside guard and corresponding accept)");
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

            when K_Generic =>
               Append (Message, ", generic");
               Usage_Message (True_Usage (K_Used), "instantiated");
         end case;

         -- Do actual report

         if Rule_Table (Value.Entity,
                        True_Usage (K_From_Visible),
                        True_Usage (K_From_Private),
                        True_Usage (K_From_Task_Guard),
                        True_Usage (K_Initialized),
                        True_Usage (K_Read),
                        True_Usage (K_Written)).Used_Controls (Check)
         then
            Report (Rule_Id,
                    To_Wide_String (Rule_Table (Value.Entity,
                                                True_Usage (K_From_Visible),
                                                True_Usage (K_From_Private),
                                                True_Usage (K_From_Task_Guard),
                                                True_Usage (K_Initialized),
                                                True_Usage (K_Read),
                                                True_Usage (K_Written)).Labels (Check)),
                    Check,
                    Value.Decl_Location,
                    To_Wide_String (Message));

         elsif Rule_Table (Value.Entity,
                           True_Usage (K_From_Visible),
                           True_Usage (K_From_Private),
                           True_Usage (K_From_Task_Guard),
                           True_Usage (K_Initialized),
                           True_Usage (K_Read),
                           True_Usage (K_Written)).Used_Controls (Search)
         then
            Report (Rule_Id,
                    To_Wide_String (Rule_Table (Value.Entity,
                                                True_Usage (K_From_Visible),
                                                True_Usage (K_From_Private),
                                                True_Usage (K_From_Task_Guard),
                                                True_Usage (K_Initialized),
                                                True_Usage (K_Read),
                                                True_Usage (K_Written)).Labels (Search)),
                    Search,
                    Value.Decl_Location,
                    To_Wide_String (Message));
         end if;

         -- Generate fixes after Check or Search, but before Count
         -- Do not generate fixes for From_Task_Guard, since there may be unknown usage from the guard/accept
         -- and not for instances!
         if not True_Usage (K_From_Task_Guard) and Value.Origin /= From_Instance then
            case Value.Entity is
               when K_Variable =>
                  if    not True_Usage (K_Written)
                    and     True_Usage (K_Read)        -- Otherwise it would be removable
                    and     True_Usage (K_Initialized)
                    and not Pseudo_Const
                  then
                     Fixes.Insert ("constant ", Fixes.Before, Object_Declaration_View
                                                                (Enclosing_Element (Value.Declaration)));
                  end if;

               when K_In_Out_Parameter =>
                  if not True_Usage (K_Read) then
                     Fixes.Delete (From => Get_Previous_Word_Location (Object_Declaration_View
                                                                         (Enclosing_Element (Value.Declaration)),
                                                                       Skipping => 1),
                                   To   => Get_Previous_Word_Location (Object_Declaration_View
                                                                         (Enclosing_Element (Value.Declaration))));
                  elsif not True_Usage (K_Written) then
                     Fixes.Delete (From => Get_Previous_Word_Location (Object_Declaration_View
                                                                         (Enclosing_Element (Value.Declaration))),
                                   To   => Get_Location               (Object_Declaration_View
                                                                         (Enclosing_Element (Value.Declaration))));
                  end if;

               when others =>
                  null;
            end case;
         end if;


         if Rule_Table (Value.Entity,
                        True_Usage (K_From_Visible),
                        True_Usage (K_From_Private),
                        True_Usage (K_From_Task_Guard),
                        True_Usage (K_Initialized),
                        True_Usage (K_Read),
                        True_Usage (K_Written)).Used_Controls (Count)
         then
            Report (Rule_Id,
                    To_Wide_String (Rule_Table (Value.Entity,
                                                True_Usage (K_From_Visible),
                                                True_Usage (K_From_Private),
                                                True_Usage (K_From_Task_Guard),
                                                True_Usage (K_Initialized),
                                                True_Usage (K_Read),
                                                True_Usage (K_Written)).Labels (Count)),
                    Count,
                    Value.Decl_Location,
                    "");
         end if;
      end Report_One;

      procedure Report_All is new Iterate (Report_One);

   begin  -- Finalize
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Report_All (Own_Usage);
   end Finalize;

   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Element : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries, Utilities;

      function Is_Array_Initialized (Def : Asis.Definition) return Boolean is
         -- Check if the components of the (formal) array of Def are automatically initialized
         -- Currently: only access types recognized as initialized
         Current_Definition : Asis.Definition := Def;
      begin
         loop
            Current_Definition := Component_Definition_View (Array_Component_Definition (Current_Definition));
            if Definition_Kind (Current_Definition) = An_Access_Definition then
               -- anonymous access component => access type => initialized
               return True;
            end if;
            -- Get rid of 'Base if necessary (cannot be 'Class)
            Current_Definition := Type_Declaration_View (Corresponding_Name_Declaration
                                                         (Strip_Attributes
                                                          (Subtype_Simple_Name (Current_Definition))));
            exit when Type_Kind (Current_Definition)
                      not in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition
              and then Formal_Type_Kind (Current_Definition)
                      not in A_Formal_Unconstrained_Array_Definition .. A_Formal_Constrained_Array_Definition;
         end loop;
         return Type_Kind (Current_Definition) = An_Access_Type_Definition
                or else Formal_Type_Kind (Current_Definition) = A_Formal_Access_Type_Definition;
      end Is_Array_Initialized;

      E_Kind : Extended_Entity_Kind;
      Origin : Package_Origins;
   begin   -- Process_Declaration
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      E_Kind := Declaration_Form (Element);
      Origin := Package_Origin   (Element);
      -- Since we are processing the declaration, the declaration scope is the current scope
      case E_Kind is
         when K_Variable =>
            -- Element is A_Variable_Declaration here

            -- Analyze the type for the case where it is an access type, or
            -- an array of accesses, since these are initialized by the compiler.
            declare
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
                           Name := Simple_Name (Prefix (Name));
                        end if;
                        Root_Definition := Type_Declaration_View (A4G_Bugs.Corresponding_First_Subtype
                                                                  (Corresponding_Name_Declaration (Name)));
                     end;
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
                              Is_Initialized := Is_Array_Initialized (Root_Definition);
                           when Not_A_Type_Definition =>
                              Failure ("Wrong definition", Root_Definition);
                           when others =>
                              Is_Initialized := False;
                        end case;
                     when An_Access_Definition =>
                        -- 2005 anonymous access
                        Is_Initialized := True;
                     when A_Formal_Type_Definition =>
                        case Formal_Type_Kind (Root_Definition) is
                           when A_Formal_Access_Type_Definition =>
                              -- Access types are always initialized
                              Is_Initialized := True;
                           when A_Formal_Unconstrained_Array_Definition
                              | A_Formal_Constrained_Array_Definition
                                =>
                              Is_Initialized := Is_Array_Initialized (Root_Definition);
                           when others =>
                              Is_Initialized := False;
                        end case;
                     when A_Private_Type_Definition
                       | A_Tagged_Private_Type_Definition
                       | A_Private_Extension_Definition
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

               for N : Asis.Defining_Name of Names (Element) loop
                  Update (N,
                          K_Variable,
                          (K_Declared     => True,
                           K_From_Visible => Origin = From_Visible,
                           K_From_Private => Origin = From_Private,
                           K_Initialized  => Is_Initialized,
                           others => False));
               end loop;
            end;

         when K_Constant =>
            for N : Asis.Defining_Name of Names (Element) loop
               Update (N,
                       K_Constant,
                       (K_Declared | K_Initialized => True,
                        K_From_Visible             => Origin = From_Visible,
                        K_From_Private             => Origin = From_Private,
                        others                     => False));
            end loop;

         when K_In_Parameter =>
            declare
               Is_Initialized  : constant Boolean := not Is_Nil (Initialization_Expression (Element));
            begin
               for N : Asis.Defining_Name of Names (Element) loop
                  Update (N,
                          E_Kind,
                          (K_Declared     => True,
                           K_From_Visible => Origin = From_Visible,
                           K_From_Private => Origin = From_Private,
                           K_Initialized  => Is_Initialized,
                           others         => False));
               end loop;
            end;

         when K_Type
            | K_Procedure
            | K_Function
            | K_Exception
            | K_Task
            | K_Protected
            | K_Generic
            | K_Out_Parameter
            | K_In_Out_Parameter
              =>
            for N : Asis.Defining_Name of Names (Element) loop
               Update (N,
                       E_Kind,
                       (K_Declared     => True,
                        K_From_Visible => Origin = From_Visible,
                        K_From_Private => Origin = From_Private,
                        others         => False));
            end loop;

         when K_Other =>
            null;
      end case;
   end Process_Declaration;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Name : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Locations, Framework.Reports, Select_Path_Stack, Thick_Queries, Utilities;

      procedure Check_Access is
         -- Checks if Name is the prefix of 'Access, 'Uncheked_Access or 'Address
         -- Calls Uncheckable if it is
         Enclosing : Asis.Element := Enclosing_Element (Name);
      begin
         while Expression_Kind (Enclosing) = A_Selected_Component loop
            Enclosing := Enclosing_Element (Enclosing);
         end loop;

         if Expression_Kind (Enclosing) = An_Attribute_Reference then
            case Attribute_Kind (Enclosing) is
               when An_Access_Attribute
                  | An_Unchecked_Access_Attribute
                  | An_Address_Attribute
                    =>
                  if Is_Part_Of_Instance (Name) then
                     -- Name may be an artificial name without a location
                     -- Put the message at the location of the instantiation
                     Uncheckable (Rule_Id,
                                  False_Negative,
                                  Get_Location (Ultimate_Enclosing_Instantiation (Name)),
                                  "Possible access to """ & Name_Image (Name) & """ through alias");
                  else
                     Uncheckable (Rule_Id,
                                  False_Negative,
                                  Get_Location (Name),
                                  "possible access through alias");
                  end if;
               when others =>
                  null;
            end case;
         end if;
      end Check_Access;

      function Name_Used_In_Guard (Of_Path : Asis.Path) return Boolean is
         Curs      : Cursor := First (Guard_Elements);
         Rec       : Guard_Record;
         Good_Name : constant Asis.Name := Ultimate_Name (Corresponding_Name_Definition (Name), No_Component => True);
      begin
         while Has_Element (Curs) loop
            Rec := Fetch (Curs);
            if Is_Equal (Rec.Guard_Elem, Good_Name) and Is_Equal (Rec.Guard_Path, Of_Path) then
               return True;
            end if;
            Curs := Next (Curs);
         end loop;
         return False;
      end Name_Used_In_Guard;

      E_Kind           : Extended_Entity_Kind;
      Is_Part_Of_Guard : Boolean := False;
      Path_Guard       : Asis.Path;
      Good_Name        : Asis.Expression;
      Item             : Asis.Element;

   begin  -- Process_Identifier
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- If name is an actual in-out of a generic instantiation, do nothing
      -- (actual usage will be found by analyzing the expanded template)
      -- By the same token, identify if Name is part of a guard (only if at least one control asked for it)
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
               end if;

               -- If this is the identifier left of the arrow in an association, it is not a usage
               -- except in an array aggregate association where it is a Read!
               case Association_Kind (Item) is
                  when A_Pragma_Argument_Association =>
                     -- Ignore pragmas (even on the RHS of the arrow)
                     return;
                  when A_Discriminant_Association =>
                     for Choice :  Asis.Expression of Discriminant_Selector_Names (Item) loop
                        if Is_Equal (Name, Choice) then
                           return;
                        end if;
                     end loop;
                  when A_Record_Component_Association =>
                     for Choice :  Asis.Expression of Record_Component_Choices (Item) loop
                        if Is_Equal (Name, Choice) then
                           return;
                        end if;
                     end loop;
                  when An_Array_Component_Association =>
                     null;
                  when A_Parameter_Association | A_Generic_Association =>
                     if Is_Equal (Name, Formal_Parameter (Item)) then
                        return;
                     end if;
                  when Not_An_Association =>
                     Failure ("Usage: not an association", Item);
               end case;

               if not From_Guard_Checked then
                  exit;
               end if;

               -- We must continue to go up until we find a select (or or) path, or anything else
               Item := Enclosing_Element (Item);

            when A_Path =>
               -- We can reach this only if From_Guard_Checked is True
               case Path_Kind (Item) is
                  when A_Select_Path | An_Or_Path =>
                     Path_Guard := Guard (Item);
                     if not Is_Nil (Path_Guard) then
                        Is_Part_Of_Guard := Is_Part_Of (Name, Inside => Path_Guard);
                        if Is_Part_Of_Guard then
                           Good_Name := Ultimate_Name (Corresponding_Name_Definition (Name), No_Component => True);
                           if not Is_Nil (Good_Name) then  -- Case of predefined operators...
                              Prepend (Guard_Elements, (Good_Name, Item));
                           end if;
                        elsif Name_Used_In_Guard (Of_Path => Item)
                          and then Is_Part_Of (Name, Inside => Thick_Queries.Statements (Item) (1))
                                  -- The first statement of the path is the accept statement
                        then
                           return;
                        end if;
                     end if;
                     exit;
                  when An_Expression_Path =>
                     Item := Enclosing_Element (Item);
                  when others =>
                     exit;
               end case;

            when A_Statement =>
               if not From_Guard_Checked then
                  exit;
               end if;

               -- We must to continue to go up until we find a select (or or) path, or anything else
               Item := Enclosing_Element (Item);

            when others =>
               exit;
         end case;
      end loop;

      begin
         Good_Name := Ultimate_Name (Name, No_Component => True);
      exception
         when Asis.Exceptions.ASIS_Failed =>
            -- Due to an ASIS-for-GNAT bug, Ultimate_Name fails if the parameter is a generic
            -- formal "in" parameter. However, if this parameter corresponds to a variable,
            -- it has already been marked as read at the level of the instantiation.
            -- Therefore, we can catch and ignore this case.
            A4G_Bugs.Trace_Bug ("Rules.Usage.Process_Identifier: ASIS_Failed");
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
                         "Name """ & Name_Image (Name) & """ is dynamic renaming");
         else
            Uncheckable (Rule_Id,
                         False_Negative,
                         Get_Location (Name),
                         "Name is dynamic renaming");
         end if;
         return;
      elsif Expression_Kind (Good_Name) = An_Attribute_Reference then
         -- Renaming of an attribute: not something we check
         return;
      end if;

      E_Kind := Declaration_Form (Good_Name);
      case E_Kind is
         when K_Variable =>
            case Expression_Usage_Kind (Name) is
               when Untouched =>
                  Check_Access;
               when Read =>
                  Update (Good_Name, K_Variable, Value => (K_Read => not Is_Part_Of_Guard,
                                                           K_From_Task_Guard => Is_Part_Of_Guard,
                                                           others            => False));
               when Write =>
                  Update (Good_Name, K_Variable, Value => (K_Written => not Is_Part_Of_Guard,
                                                           K_From_Task_Guard => Is_Part_Of_Guard,
                                                           others    => False));
               when Read_Write =>
                  Update (Good_Name, K_Variable, Value => (K_Read | K_Written => not Is_Part_Of_Guard,
                                                           K_From_Task_Guard  => Is_Part_Of_Guard,
                                                           others             => False));
               when Unknown =>      -- Consider Unknown as Read-Write, therefore creating false positives
                                    -- That's better than false negatives!
                  Uncheckable (Rule_Id,
                               False_Positive,
                               Get_Location (Name),
                               "variable """ & Name_Image (Good_Name)
                               & """ used as parameter of dispatching call, treated as in-out");
                  Update (Good_Name, K_Variable, Value => (K_Read | K_Written => not Is_Part_Of_Guard,
                                                           K_From_Task_Guard  => Is_Part_Of_Guard,
                                                           others             => False));
            end case;

         when K_Constant =>
            case Expression_Usage_Kind (Name) is
               when Untouched =>
                  Check_Access;
               when Read
                  | Unknown   -- For a constant, it is safe to assume that the only possible usage is "read"
                  =>
                  Update (Good_Name, K_Constant, Value => (K_Read            => not Is_Part_Of_Guard,
                                                           K_From_Task_Guard => Is_Part_Of_Guard,
                                                           others            => False));
               when Write | Read_Write =>
                  Failure ("Usage: write of constant", Name);
            end case;

         when K_In_Parameter =>
            case Expression_Usage_Kind (Name) is
               when Untouched =>
                  Check_Access;
               when Read | Unknown => -- Unknown : IN parameter so the only possible usage is "read"
                  Update (Good_Name, K_In_Parameter, Value => (K_Read            => not Is_Part_Of_Guard,
                                                               K_From_Task_Guard => Is_Part_Of_Guard,
                                                               others            => False));
               when Write | Read_Write =>
                  Failure ("Usage: write of IN parameter", Name);
            end case;

         when K_Out_Parameter | K_In_Out_Parameter =>
            case Expression_Usage_Kind (Name) is
               when Untouched =>
                  Check_Access;
               when Read =>
                  Update (Good_Name, E_Kind, Value => (K_Read            =>  not Is_Part_Of_Guard,
                                                       K_From_Task_Guard => Is_Part_Of_Guard,
                                                       others            => False));
               when Write =>
                  Update (Good_Name, E_Kind, Value => (K_Written         =>  not Is_Part_Of_Guard,
                                                       K_From_Task_Guard => Is_Part_Of_Guard,
                                                       others            => False));
               when Read_Write =>
                  Update (Good_Name, E_Kind, Value => (K_Read | K_Written =>  not Is_Part_Of_Guard,
                                                       K_From_Task_Guard  => Is_Part_Of_Guard,
                                                       others             => False));
               when Unknown =>      -- Consider Unknown as Read-Write, therefore creating false positives
                  -- That's better than false negatives!
                  Uncheckable (Rule_Id,
                               False_Positive,
                               Get_Location (Name),
                               "parameter """ & Name_Image (Good_Name)
                               & """ used as parameter of dispatching call, treated as in-out");
                  Update (Good_Name, E_Kind, Value => (K_Read | K_Written => not Is_Part_Of_Guard,
                                                       K_From_Task_Guard  => Is_Part_Of_Guard,
                                                       others             => False));
            end case;

         when K_Procedure | K_Function =>
            declare
               Enclosing : Asis.Element := Enclosing_Element (Name);
            begin
               while Expression_Kind (Enclosing) = A_Selected_Component loop
                  Enclosing := Enclosing_Element (Enclosing);
               end loop;

               case Element_Kind (Enclosing) is
                  when An_Expression =>
                     case Expression_Kind (Enclosing) is
                        when An_Attribute_Reference =>
                           -- We should check that it is really 'Address or 'Access, but what else could it be?
                           Update (Good_Name, E_Kind, Value => (K_Accessed        => not Is_Part_Of_Guard,
                                                                K_From_Task_Guard => Is_Part_Of_Guard,
                                                                others            => False));
                        when A_Function_Call =>
                           Update (Good_Name, E_Kind, Value => (K_Called          => not Is_Part_Of_Guard,
                                                                K_From_Task_Guard => Is_Part_Of_Guard,
                                                                others            => False));
                        when others =>
                           null;
                     end case;
                  when A_Statement =>
                     case Statement_Kind (Enclosing) is
                        when A_Procedure_Call_Statement =>
                           -- Can't be part of a guard....
                           Update (Good_Name, E_Kind, Value => (K_Called => True, others => False));
                        when others =>
                           null;
                     end case;
                  when others =>
                     null;
               end case;
            end;

         when K_Exception =>
            declare
               Enclosing : Asis.Element := Enclosing_Element (Name);
            begin
               while Expression_Kind (Enclosing) = A_Selected_Component loop
                  Enclosing := Enclosing_Element (Enclosing);
               end loop;

               if Expression_Kind (Enclosing) = An_Attribute_Reference
                 and then Attribute_Kind (Enclosing) = An_Identity_Attribute
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
                           -- Can't be in a guard, since it is a parameter to a procedure call here
                           Update (Good_Name, K_Exception, Value => (K_Raised => True, others => False));
                        end if;
                     end if;
                  end if;

               elsif Statement_Kind (Enclosing) = A_Raise_Statement then
                  -- Can't be in a guard here
                  Update (Good_Name, K_Exception, Value => (K_Raised => True, others => False));

               elsif Expression_Kind (Enclosing) = A_Raise_Expression then
                  -- Can be in a guard here (although pretty weird)
                  Update (Good_Name, K_Exception, Value => (K_Raised          => not Is_Part_Of_Guard,
                                                            K_From_Task_Guard => Is_Part_Of_Guard,
                                                            others            => False));

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
                 and then Attribute_Kind (Enclosing) = An_Identity_Attribute
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
                           -- Can't be in a guard here
                           Update (Good_Name, K_Task, Value => (K_Aborted => True, others => False));
                        end if;
                     end if;
                  end if;

               else
                  case Statement_Kind (Enclosing) is
                     when An_Abort_Statement =>
                        -- Can't be in a guard here
                        Update (Good_Name, K_Task, Value => (K_Aborted => True, others => False));
                     when An_Entry_Call_Statement =>
                        -- Can't be in a guard here
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
                        Update (Good_Name, K_Protected, Value => (K_Called          => not Is_Part_Of_Guard,
                                                                  K_From_Task_Guard => Is_Part_Of_Guard,
                                                                  others            => False));
                     end if;
                  when An_Entry_Call_Statement | A_Procedure_Call_Statement =>
                     -- Can't be in a guard here
                     Update (Good_Name, K_Protected, Value => (K_Called => True, others => False));
                  when others =>
                     null;
               end case;
            end;

         when K_Type =>
            Update (Good_Name, K_Type, Value => (K_Used => True, others => False));

         when K_Generic =>
            Update (Good_Name, K_Generic, Value => (K_Instantiated => True, others => False));

         when K_Other =>
            null;
      end case;
   end Process_Identifier;


   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Instantiation : Asis.Declaration) is
      use Asis, Asis.Elements, Asis.Declarations;
      use Thick_Queries;

      The_Info    : Null_State;
      The_Control : Traverse_Control := Continue;

      procedure Pre_Procedure (Element : in     Asis.Element;
                               Control : in out Asis.Traverse_Control;
                               State   : in out Null_State);

      procedure Traverse is new Asis.Iterator.Traverse_Element
        (Null_State, Pre_Procedure, Null_State_Procedure);

      procedure Pre_Procedure (Element : in     Asis.Element;
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
                     Traverse (Prefix (Element), Control, State);
                     Control := Abandon_Children;
                  when others =>
                     null;
               end case;

            when A_Declaration =>
               case Declaration_Kind (Element) is
                  when A_Variable_Declaration
                     | A_Constant_Declaration
                     | A_Deferred_Constant_Declaration
                     | A_Number_Declaration
                     | A_Subtype_Declaration
                       =>
                     -- Do not consider pseudo declarations of constants, variables, or subtypes that
                     -- correspond, in the expanded template, to the formals of the generic.
                     begin
                        case Declaration_Kind (Enclosing_Element (Corresponding_Generic_Element
                                                                  (Names (Element) (1))))
                        is
                           when A_Formal_Object_Declaration
                              | A_Formal_Type_Declaration
                                =>
                              Control := Abandon_Children;
                           when others =>
                              Process_Declaration (Element);
                        end case;
                     exception
                        when Asis.Exceptions.Asis_Inappropriate_Element =>
                        -- See comment in Package_Origin, the same can happen here
                        -- TBSL: this is really a work-around
                        Control := Abandon_Children;
                     end;
                  when An_Ordinary_Type_Declaration
                     | A_Procedure_Declaration
                     | A_Null_Procedure_Declaration
                     | A_Procedure_Body_Declaration
                     | A_Procedure_Body_Stub
                     | A_Function_Declaration
                     | An_Expression_Function_Declaration   -- Ada 2012
                     | A_Function_Body_Declaration
                     | A_Function_Body_Stub
                     | A_Single_Task_Declaration
                     | A_Single_Protected_Declaration
                     =>
                     Process_Declaration (Element);
                  when others =>
                     -- Since we are in an instantiated unit, all subinstantiations are expanded
                     -- there is therefore no need to handle instantiations here
                     null;
               end case;

            when A_Definition =>
               case Definition_Kind (Element) is
                  when An_Aspect_Specification =>
                     -- 2012, ignored for the moment
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
      end Pre_Procedure;

      use Framework.Rules_Manager;
      Generic_Name : Asis.Expression;
   begin  -- Process_Instantiation
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Process_Declaration (Instantiation);

      -- Do not check instantiations from the standard library or banned units
      Generic_Name := Simple_Name (Generic_Unit_Name (Instantiation));
      if Ultimate_Origin (Generic_Name) /=  An_Application_Unit
         or Is_Banned (Generic_Name, For_Rule => Rule_Id)
      then
         return;
      end if;

      declare
         Instantiated_Spec : constant Asis.Declaration := Corresponding_Declaration (Instantiation);
         Instantiated_Body : constant Asis.Declaration := Corresponding_Body        (Instantiated_Spec);
         -- Note: it would seem better to use Corresponding_Body (Instantiation), but this seems to return
         -- Nil_Element, in hardly reproduceable manner. See Ticket #0000029
      begin
         Traverse (Instantiated_Spec, The_Control, The_Info);

         if not Is_Nil (Instantiated_Body) then
            Traverse (Instantiated_Body, The_Control, The_Info);
         end if;
      end;
   end Process_Instantiation;


   -----------------------
   -- Post_Process_Path --
   -----------------------

   procedure Post_Process_Path (Path : Asis.Path) is
   -- Clean-up Guard_Elements for this path. Since it is managed as a stack, removed elements are at the top
      use Select_Path_Stack;
      use Asis.Elements;

      Curs : Cursor := First (Guard_Elements);
   begin
      while Has_Element (Curs) and then Is_Equal (Fetch (Curs).Guard_Path, Path) loop
         Clear (Guard_Elements, 1);
         Curs := First (Guard_Elements);
      end loop;
   end Post_Process_Path;

begin  -- Rules.Usage
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB         => Help'Access,
                                     Add_Control_CB  => Add_Control'Access,
                                     Command_CB      => Command'Access,
                                     Finalize_CB     => Finalize'Access);
end Rules.Usage;
