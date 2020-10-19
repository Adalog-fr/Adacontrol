----------------------------------------------------------------------
--  Rules.Barrier_Expressions - Package body                        --
--                                                                  --
--  This software is (c) Adalog 2004-2005.                          --
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
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- AdaControl
with
   Framework.Language;
pragma Elaborate (Framework.Language);

package body Rules.Barrier_Expressions is
   use Framework, Framework.Control_Manager;

   type Keyword is (K_Entity,              K_Allocation,          K_Any_Component,
                    K_Any_Variable,        K_Arithmetic_Operator, K_Array_Aggregate,
                    K_Comparison_Operator, K_Conversion,          K_Dereference,
                    K_Indexing,            K_Function_Attribute,  K_Local_Function,
                    K_Logical_Operator,    K_Record_Aggregate,    K_Value_Attribute);
   package Keyword_Flag_Utilities is new Framework.Language.Flag_Utilities (Keyword, "K_");

   -- In the following record, Types (K) is true if the check must be performed for K,
   -- i.e. the <entity> is /not/ allowed for K
   type Key_Context is new Root_Context with
      record
         Types : Control_Kinds_Set;
      end record;
   Contexts  : Context_Store;

   Rule_Used : Control_Kinds_Set := (others => False);
   Save_Used : Control_Kinds_Set;
   Labels    : array (Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities, Keyword_Flag_Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control constucts used in protected entry barriers");
      User_Message;
      Help_On_Flags ("Parameter(s):", Extra_Value => "<entity>");
   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language, Keyword_Flag_Utilities, Utilities;

      Key  : Keyword;
      Spec : Entity_Specification;
      Cont : Key_Context;
   begin
      if Rule_Used (Ctl_Kind) then
         Parameter_Error (Rule_Id,  "rule already specified for " & To_Lower (Control_Kinds'Wide_Image (Ctl_Kind)));
      end if;
      Cont.Types            := (others => True);
      Cont.Types (Ctl_Kind) := False;

      while Parameter_Exists loop
         Key := Get_Flag_Parameter (Allow_Any => True);

         if Key = K_Entity then
            Spec := Get_Entity_Parameter;
         else
            Spec := Value (Image (Key));
         end if;

         begin
            Associate (Contexts, Spec, Cont);
         exception
            when Already_In_Store =>
               Cont := Key_Context (Association (Contexts, Spec));
               if Cont.Types (Ctl_Kind) then
                  Cont.Types (Ctl_Kind) := False;
                  Update (Contexts, Cont);
               else
                  Parameter_Error (Rule_Id, "parameter already provided for "
                                     & To_Lower (Control_Kinds'Wide_Image (Ctl_Kind))
                                     & ": " & Image (Spec));
               end if;
         end;
      end loop;

      Labels    (Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used (Ctl_Kind) := True;
   end Add_Control;


   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
            Clear (Contexts);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -------------------------------
   -- Process_Entry_Declaration --
   -------------------------------

   procedure Process_Entry_Declaration (Decl : in Asis.Declaration) is
      use Asis.Declarations;

      procedure Check_Expression (Exp : in Asis.Expression) is
         use Asis, Asis.Definitions, Asis.Elements, Asis.Expressions;
         use Framework.Locations, Keyword_Flag_Utilities, Thick_Queries, Utilities;

         procedure Do_Report (Message    : in Wide_String;
                              Context    : in Root_Context'Class;
                              Identifier : in Asis.Element := Nil_Element;
                              Loc        : in Location := Get_Location (Exp))
         is
            use Framework.Reports, Ada.Strings.Wide_Unbounded;
            S : Control_Kinds_Set;
         begin
            if Context = No_Matching_Context then
               if Is_Nil (Identifier) then
                  S := Rule_Used;
               else
                  declare
                     Alternate_Context : constant Root_Context'Class
                       := Matching_Context (Contexts, Identifier, Extend_To => All_Extensions);
                  begin
                     if Alternate_Context = No_Matching_Context then
                        S := Rule_Used;
                     else
                        S := Key_Context (Alternate_Context).Types and Rule_Used;
                     end if;
                  end;
               end if;
            else
               S := Key_Context (Context).Types and Rule_Used;
            end if;

            if S (Check) then
               Report (Rule_Id, To_Wide_String (Labels (Check)), Check, Loc, Message);
            elsif S (Search) then
               Report (Rule_Id, To_Wide_String (Labels (Search)), Search, Loc, Message);
            end if;

            if S (Count) then
               Report (Rule_Id, To_Wide_String (Labels (Count)), Count, Loc, "");
            end if;
         end Do_Report;

      begin   -- Check_Expression
         case Expression_Kind (Exp) is
            when Not_An_Expression =>
               Failure (Rule_Id & ": Not_An_Expression");

            when An_Identifier =>
               declare
                  Name_Decl  : Asis.Declaration;
               begin
                  for N : Asis.Expression of Used_Identifiers (Exp) loop
                     Name_Decl  := Corresponding_Name_Declaration (N);
                     case Declaration_Kind (Name_Decl) is
                        when A_Package_Declaration
                           | A_Package_Body_Declaration
                           | A_Package_Renaming_Declaration
                           =>
                           -- Can appear only as prefix => Harmless
                           null;
                        when A_Function_Declaration
                           | An_Expression_Function_Declaration   -- Ada 2012
                           | A_Function_Body_Declaration
                           | A_Function_Renaming_Declaration
                           =>
                           -- Can be a call or a prefix, but the case of the call was handled as
                           -- A_Function_Call
                           null;
                        when A_Type_Declaration
                           | A_Subtype_Declaration
                           =>
                           -- Can appear in a membership choice list
                           null;
                        when A_Variable_Declaration
                           | An_Object_Renaming_Declaration
                           | A_Single_Protected_Declaration
                           | A_Loop_Parameter_Specification  -- Consider this (and next) as variables,
                           | An_Entry_Index_Specification    -- although they are strictly speaking constants
                           =>
                           Do_Report ("variable",
                                      Control_Manager.Association (Contexts, Image (K_Any_Variable)),
                                      N);
                        when A_Component_Declaration =>
                           -- This can be:
                           --   A component of a protected element: boolean fields always allowed, others checked
                           --   A component of a record type: nothing to check, the check is performed on the data
                           --      that encloses the component.
                           if Definition_Kind (Enclosing_Element (Name_Decl)) = A_Protected_Definition then
                              -- A field of the protected element, boolean fields always allowed
                              if To_Upper (Full_Name_Image
                                           (Subtype_Simple_Name
                                            (Component_Definition_View
                                             (Object_Declaration_View (Name_Decl)))))
                                  /= "STANDARD.BOOLEAN"
                              then
                                 Do_Report ("non-boolean protected component",
                                            Control_Manager.Association (Contexts, Image (K_Any_Component)),
                                            N);
                              end if;
                           end if;
                        when A_Discriminant_Specification =>
                           -- Handle like a component (see comment above)
                           if Declaration_Kind (Enclosing_Element (Enclosing_Element (Name_Decl)))
                             = A_Protected_Type_Declaration
                           then
                              -- A field of the protected element, boolean fields always allowed
                              if To_Upper (Full_Name_Image
                                           (Strip_Attributes -- In case someone tries to fool us with Boolean'Base...)
                                            (Simple_Name
                                             (Object_Declaration_View (Name_Decl)))))
                                /= "STANDARD.BOOLEAN"
                              then
                                 Do_Report ("non-boolean protected discriminant",
                                            Control_Manager.Association (Contexts, Image (K_Any_Component)),
                                            N);
                              end if;
                           end if;
                        when A_Constant_Declaration
                           | A_Number_Declaration
                             =>
                           -- always allowed
                           null;
                        when others =>
                           Failure (Rule_Id
                                    & ": unexpected declaration kind "
                                    & Declaration_Kinds'Wide_Image (Declaration_Kind (Name_Decl)),
                                    N);
                     end case;
                  end loop;
               end;
            when An_Integer_Literal
               | A_String_Literal
               | A_Real_Literal
               | A_Character_Literal
               | An_Enumeration_Literal
               | A_Null_Literal
               =>
               -- always allowed
               null;

            when An_Operator_Symbol =>
               -- Already handled as A_Function_Call
               null;

            when An_Attribute_Reference =>
               if Is_Callable_Construct (Exp) then
                  Do_Report ("callable attribute",
                             Control_Manager.Association (Contexts, Image (K_Function_Attribute)),
                             Exp);
               else
                  Do_Report ("value attribute",
                             Control_Manager.Association (Contexts, Image (K_Value_Attribute)),
                             Exp);
               end if;

            when An_And_Then_Short_Circuit
              | An_Or_Else_Short_Circuit
              =>
               Do_Report ("short-circuit control form",
                          Control_Manager.Association (Contexts, Image (K_Logical_Operator)),
                          Loc => Get_Next_Word_Location (Short_Circuit_Operation_Left_Expression (Exp)));

               -- Check left and right expressions
               Check_Expression (Short_Circuit_Operation_Left_Expression (Exp));
               Check_Expression (Short_Circuit_Operation_Right_Expression (Exp));

            when A_Parenthesized_Expression =>
               -- Check the expression within parenthesis
               Check_Expression (Expression_Parenthesized (Exp));

            when A_Record_Aggregate =>
               Do_Report ("record aggregate",
                          Control_Manager.Association (Contexts, Image (K_Record_Aggregate)));

               -- Record_Component_Associations + Record_Component_Choices/Component_Expression
               for Assoc : Asis.Association of Record_Component_Associations (Exp) loop
                  Check_Expression (Component_Expression (Assoc));
               end loop;

            when An_Extension_Aggregate =>
               Do_Report ("record extension",
                          Control_Manager.Association (Contexts, Image (K_Record_Aggregate)));

               -- Extension_Aggregate_Expression
               -- Record_Component_Associations + Record_Component_Choices/Component_Expression
               Check_Expression (Extension_Aggregate_Expression (Exp));
               for Assoc : Asis.Association of  Record_Component_Associations (Exp) loop
                  Check_Expression (Component_Expression (Assoc));
               end loop;

            when A_Positional_Array_Aggregate
              | A_Named_Array_Aggregate
              =>
               Do_Report ("array aggregate",
                          Control_Manager.Association (Contexts, Image (K_Array_Aggregate)));

               -- Array_Component_Associations + Array_Component_Choices/Component_Expression
               for Assoc : Asis.Association of Array_Component_Associations (Exp) loop
                  for Choice : Asis.Element of Array_Component_Choices (Assoc) loop
                     if not Is_Nil (Choice) then
                        case Element_Kind (Choice) is
                           when An_Expression =>
                              Check_Expression (Choice);
                           when A_Definition =>
                              case Definition_Kind (Choice) is
                                 when An_Others_Choice =>
                                    null;
                                 when A_Discrete_Range =>
                                    case Discrete_Range_Kind (Choice) is
                                       when Not_A_Discrete_Range =>
                                          Failure (Rule_Id & ": Array_Aggregate . Discrete_Range_Kind");
                                       when A_Discrete_Subtype_Indication
                                          | A_Discrete_Range_Attribute_Reference
                                          =>
                                          null;
                                       when A_Discrete_Simple_Expression_Range =>
                                          Check_Expression (Lower_Bound (Choice));
                                          Check_Expression (Upper_Bound (Choice));
                                    end case;
                                 when others =>
                                    Failure (Rule_Id & ": Array_Aggregate . Definition_Kind");
                              end case;
                           when others =>
                              Failure (Rule_Id & ": Array_Aggregate . Element_Kind");
                        end case;
                     end if;
                  end loop;
                  Check_Expression (Component_Expression (Assoc));
               end loop;

            when An_In_Membership_Test
              | A_Not_In_Membership_Test
              =>
               Do_Report ("membership test",
                          Control_Manager.Association (Contexts, Image (K_Logical_Operator)),
                          Loc => Get_Next_Word_Location (Membership_Test_Expression (Exp)));

               -- Check both tested expression and each membership choice
               Check_Expression (Membership_Test_Expression (Exp));
               for Choice : Asis.Element of Membership_Test_Choices (Exp) loop
                  if Element_Kind (Choice) = An_Expression then
                     Check_Expression (Choice);
                  else
                     -- A range
                     case Constraint_Kind (Choice) is
                        when A_Range_Attribute_Reference =>
                           null;
                        when A_Simple_Expression_Range =>
                           Check_Expression (Lower_Bound (Choice));
                           Check_Expression (Upper_Bound (Choice));
                        when others =>
                           Failure (Rule_Id & ": Membership_Test_Range => invalid Constraint_Kind");
                     end case;
                  end if;
               end loop;

            when An_Indexed_Component =>
               Do_Report ("indexing",
                          Control_Manager.Association (Contexts, Image (K_Indexing)));

               -- Check for implicit dereference
               if Is_Access_Expression (Prefix (Exp)) then
                Do_Report ("dereference",
                           Control_Manager.Association (Contexts, Image (K_Dereference)));
               end if;
               -- Check both prefix and indexes of the component
               Check_Expression (Prefix (Exp));
               for Index : Asis.Expression of Index_Expressions (Exp) loop
                  Check_Expression (Index);
               end loop;

            when A_Slice =>
               Do_Report ("slice",
                          Control_Manager.Association (Contexts, Image (K_Indexing)));

                -- Check for implicit dereference
               if Is_Access_Expression (Prefix (Exp)) then
                Do_Report ("dereference",
                           Control_Manager.Association (Contexts, Image (K_Dereference)));
               end if;                                          -- Check both slice prefix and range
               Check_Expression (Prefix (Exp));
               declare
                  The_Range : constant Asis.Discrete_Range := Slice_Range (Exp);
               begin
                  case Discrete_Range_Kind (The_Range) is
                     when Not_A_Discrete_Range =>
                        Failure (Rule_Id & ": Slice_Range => Not_A_Discrete_Range");
                     when A_Discrete_Subtype_Indication
                       | A_Discrete_Range_Attribute_Reference
                       =>
                        null;
                     when A_Discrete_Simple_Expression_Range =>
                        Check_Expression (Lower_Bound (The_Range));
                        Check_Expression (Upper_Bound (The_Range));
                  end case;
               end;

            when A_Selected_Component =>
               -- Check for implicit dereference
               if Is_Access_Expression (Prefix (Exp)) then
                  Do_Report ("dereference",
                             Control_Manager.Association (Contexts, Image (K_Dereference)));
               end if;
               -- Check both prefix and selector
               Check_Expression (Prefix (Exp));
               Check_Expression (Selector (Exp));

            when A_Function_Call =>
               -- Checks about the call itself
               declare
                  Called : constant Call_Descriptor := Corresponding_Call_Description (Exp);
               begin
                  case Called.Kind is
                     when A_Regular_Call =>
                        if Definition_Kind (Enclosing_Element (Called.Declaration)) = A_Protected_Definition
                          and then Is_Nil (External_Call_Target (Exp))
                        then
                           -- It is a call to a protected function of the same PO
                           Do_Report ("local function call",
                                      Control_Manager.Association (Contexts, Image (K_Local_Function)),
                                      Called_Simple_Name (Exp));
                        else
                           Do_Report ("non-local function call", Matching_Context (Contexts,
                                                                                   Called_Simple_Name (Exp),
                                                                                   Extend_To => All_Extensions));
                        end if;
                     when A_Predefined_Entity_Call =>
                        case Operator_Kind (Called_Simple_Name (Exp)) is
                           when Not_An_Operator =>
                              Failure (Rule_Id & ": Not_An_Operator");
                           when An_And_Operator
                              | An_Or_Operator
                              | An_Xor_Operator
                              | A_Not_Operator
                                =>
                              Do_Report ("predefined logical operator",
                                         Control_Manager.Association (Contexts, Image (K_Logical_Operator)),
                                         Loc => Get_Location (Prefix (Exp)));
                           when An_Equal_Operator
                              | A_Not_Equal_Operator
                              | A_Less_Than_Operator
                              | A_Less_Than_Or_Equal_Operator
                              | A_Greater_Than_Operator
                              | A_Greater_Than_Or_Equal_Operator
                                =>
                              Do_Report ("predefined comparison operator",
                                         Control_Manager.Association (Contexts, Image (K_Comparison_Operator)),
                                         Loc => Get_Location (Prefix (Exp)));
                           when others =>
                              Do_Report ("predefined arithmetic operator",
                                         Control_Manager.Association (Contexts, Image (K_Arithmetic_Operator)),
                                         Loc => Get_Location (Prefix (Exp)));
                        end case;

                     when An_Attribute_Call =>
                        -- Will handle it when traversing the prefix (because we need to handle value
                        -- attributes anyway)
                        null;

                     when A_Dereference_Call =>
                        Do_Report ("dereference",
                                   Control_Manager.Association (Contexts, Image (K_Dereference)));

                     when A_Dispatching_Call =>
                        -- Same as regular call
                        Do_Report ("non-local function call", Matching_Context (Contexts, Called_Simple_Name (Exp)));

                     when An_Enumeration_Literal =>
                        -- Allways allowed
                        null;
                  end case;

               end;
               -- Check prefix
               Check_Expression (Prefix (Exp));
               -- Check each parameter
               for Parameter : Asis.Association of Function_Call_Parameters (Exp) loop
                  Check_Expression (Actual_Parameter (Parameter));
               end loop;

            when An_Explicit_Dereference =>
               Do_Report ("dereference",
                          Control_Manager.Association (Contexts, Image (K_Dereference)));
               Check_Expression (Prefix (Exp));

            when A_Type_Conversion
              | A_Qualified_Expression
              =>
               Do_Report ("conversion or qualified expression",
                          Control_Manager.Association (Contexts, Image (K_Conversion)));
               Check_Expression (Converted_Or_Qualified_Expression (Exp));

            when An_Allocation_From_Subtype =>
               Do_Report ("allocation",
                          Control_Manager.Association (Contexts, Image (K_Allocation)));

            when An_Allocation_From_Qualified_Expression =>
               Do_Report ("allocation",
                          Control_Manager.Association (Contexts, Image (K_Allocation)));
               Check_Expression (Allocator_Qualified_Expression (Exp));

            pragma Warnings(Off); -- others covers nothing for versions of gnat that do not support the extension
            when others =>
               -- Corresponds to GNAT extension: A_Conditional_Expression
               Reports.Uncheckable (Rule_Id, False_Negative, Get_Location (Exp), "Use of compiler specific extension");
            pragma Warnings (On);
         end case;
      end Check_Expression;


   begin -- Process_Entry_Declaration
      if Rule_Used = (Control_Kinds => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Expression (Entry_Barrier (Decl));
   end Process_Entry_Declaration;

begin  -- Rules.Barrier_Expressions
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Barrier_Expressions;
