----------------------------------------------------------------------
--  Rules.Style - Package body                                      --
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

-- Ada
with
   Ada.Strings.Wide_Fixed,
   Ada.Strings.Wide_Unbounded,
   System;

-- ASIS
with
  Asis.Clauses,
  Asis.Compilation_Units,
  Asis.Elements,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Expressions,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  Scope_Manager,
  String_Matching,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Reports.Fixes,
  Framework.Language,
  Framework.Ordering_Machine,
  Rules.Style.Keyword;
pragma Elaborate (Framework.Language);
pragma Elaborate (Framework.Ordering_Machine);

package body Rules.Style is
   use Framework, Framework.Control_Manager, Utilities;
   use type Thick_Queries.Biggest_Int;

   -- See declaration of Style_Names in the private part of the specification
   subtype Casing_Styles is Subrules range St_Casing_Aspect .. St_Casing_Pragma;
   type Usage_Flags is array (Subrules) of Boolean;
   No_Subrule : constant Usage_Flags := (others => False);

   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Subrules,
                                                                             Prefix => "St_" );

   -------------------------------------------------------------------------
   --
   -- Declarations for the casing subrule
   --                      ******
   --

   -- See declaration of Casing_Names in the private part of the specification
   package Casing_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Casing_Names,
                                                                           Prefix => "Ca_" );
   Casing_Policy : array (Casing_Styles) of Casing_Set := (others => (others => False));
   -- We use a simple variable here rather than storing into the context,
   -- because the rule can be given only once, and efficiency is a concern
   -- (the rule is called on every identifier).


   -------------------------------------------------------------------------
   --
   -- Declarations for the compound_statement subrule
   --                      ******************
   --

   Min_Stat_Length : constant array (Asis.Statement_Kinds) of Asis.Text.Line_Number
     := (Asis.An_If_Statement                    => 3,
         Asis.A_Case_Statement                   => 4,
         Asis.A_Loop_Statement                   => 3,
         Asis.A_While_Loop_Statement             => 3,
         Asis.A_For_Loop_Statement               => 3,
         Asis.A_Block_Statement                  => 3,
         Asis.An_Accept_Statement                => 3, -- Only if the accept has a body
         Asis.A_Selective_Accept_Statement       => 4,
         Asis.A_Timed_Entry_Call_Statement       => 4,
         Asis.A_Conditional_Entry_Call_Statement => 5,
         Asis.An_Asynchronous_Select_Statement   => 5,
         others                                  => 1);


   -------------------------------------------------------------------------
   --
   -- Declarations for the exposed_literal subrule
   --                      ***************
   --

   type Literal_Names is (Lit_Integer, Lit_Real, Lit_Character, Lit_String);
   package Literal_Flag_Utilities  is new Framework.Language.Flag_Utilities (Flags => Literal_Names,
                                                                             Prefix => "Lit_" );
   type Place_Names is (Pl_Other,
                        Pl_Declaration,
                        Pl_Constant, Pl_Number,      Pl_Var_Init, Pl_Type,
                        Pl_Statement,
                        Pl_Pragma,   Pl_Repr_Clause, Pl_Index,    Pl_Aggr_Index, Pl_Attr_Index, Pl_Exponent);
   -- Pl_Other used internally when not in one of the other Places, not accessible to user. Must stay first.
   subtype Other_Declarations is Place_Names range Pl_Constant .. Pl_Type;

   type Place_Set is array (Place_Names) of Boolean;
   No_Place : constant Place_Set := (others => False);
   package Place_Flag_Utilities  is new Framework.Language.Flag_Utilities (Flags => Place_Names,
                                                                           Prefix => "Pl_" );

   Nbr_Of_Permitted_Consts : constant := 20;
   type Permitted_Consts_Count is range 0 .. Nbr_Of_Permitted_Consts;
   subtype Permitted_Consts_Range is Permitted_Consts_Count range 1 .. Permitted_Consts_Count'Last;

   Uninitialized : constant  := -1;

   Integer_Max_Value        : Thick_Queries.Biggest_Int := Uninitialized;
   Integer_Permitted_Values : array (Permitted_Consts_Range) of Thick_Queries.Biggest_Int;
   Real_Permitted_Values    : array (Permitted_Consts_Range) of Float;
   String_Permitted_Values  : array (Permitted_Consts_Range) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Integer_Count            : Permitted_Consts_Count := 0;     -- Number of Integer parameters
   Real_Count               : Permitted_Consts_Count := 0;     -- Number of Real parameters
   String_Count             : Permitted_Consts_Count := 0;     -- Number of String parameters
   Permitted_Places         : array (Literal_Names) of Place_Set := (others => (others => False));


   -------------------------------------------------------------------------
   --
   -- Declarations for the literal subrule
   --                      *******
   --

   subtype Allowed_Bases is Asis.ASIS_Positive range 2 .. 16;
   type Literal_Context is new Basic_Rule_Context with
      record
         Is_Not     : Boolean;
         Block_Size : Asis.ASIS_Natural;
      end record;


   -------------------------------------------------------------------------
   --
   -- Declarations for the multiple_elements subrule
   --                      *****************
   --

   type Multiple_Names is (Mu_Clause, Mu_Pragma, Mu_Declaration, Mu_Statement, Mu_Handler, -- Construct must start line
                           Mu_Begin,  Mu_End,    Mu_Then,        Mu_When,      Mu_Else,    -- Keyword must start line
                           Mu_Is,     Mu_Loop,   Mu_Do,                                    -- Keyword must start line
                                                                                           -- or be on same line
                           Mu_Keywords);  -- Special value, equivalent to all keywords
   subtype Multiple_Keywords is Multiple_Names range Mu_Begin .. Mu_Do;
   package Multiple_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags  => Multiple_Names,
                                                                             Prefix => "Mu_" );
   Flexible_Clause : Boolean;
   -- True if at least one Flexible


   -------------------------------------------------------------------------
   --
   -- Declarations for the no_closing_name subrule
   --                      ***************
   --

   type Closing_Name_Context is new Basic_Rule_Context with
      record
         Length : Asis.ASIS_Integer;
      end record;


   -------------------------------------------------------------------------
   --
   -- Declarations for the [formal_]parameter_order subrule
   --                      ************************
   --

   type Extended_Modes is (Mode_In,   Mode_Defaulted_In, Mode_Access,   Mode_In_Out, Mode_Out,
                           Mode_Type, Mode_Procedure,    Mode_Function, Mode_Package);
   package Extended_Modes_Utilities is new Framework.Language.Modifier_Utilities (Modifiers => Extended_Modes,
                                                                                  Prefix    => "Mode_");
   package Parameter_Ordering_Machine is new Framework.Ordering_Machine (Rule_Id,
                                                                         Extended_Modes,
                                                                         Extended_Modes_Utilities.Modifier_Set);
   Parameter_Ordering        : Parameter_Ordering_Machine.Instance;
   Formal_Parameter_Ordering : Parameter_Ordering_Machine.Instance;

   -------------------------------------------------------------------------
   --
   -- Declarations for the renamed_entity subrule
   --                      **************

   --

   type Renaming_Data is
      record
         Ren_Location : Locations.Location;
         Renaming_Def : Asis.Defining_Name;
         Renamed_Def  : Asis.Defining_Name;
      end record;
   function Is_Same_Def (L, R : Renaming_Data) return Boolean;
   package Renamed_Entities is new Scope_Manager.Scoped_Store (Renaming_Data, Equivalent_Keys => Is_Same_Def);
   -- Note that we cannot use Symbol_Table here, because the renamed entity is pushed
   -- in the scope of the renaming entity

   -------------------------------------------------------------------------
   --
   -- General parameters
   --

   Rule_Used : Usage_Flags := No_Subrule;
   Save_Used : Usage_Flags;

   Contexts : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control various Ada style issues");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags (Header => "Parameter(1):");
      User_Message;
      User_Message ("For casing_*:");
      Casing_Flag_Utilities.Help_On_Flags (Header => "Parameter(2..):");
      User_Message;
      User_Message ("For exposed_literal:");
      Literal_Flag_Utilities.Help_On_Flags (Header => "Parameter(2):");
      Place_Flag_Utilities.Help_On_Flags (Header     => "Parameter(3..):",
                                          Footer     => "(optional)",
                                          Extra_Value => "[max] <value>");
      User_Message;
      User_Message ("For multiple_elements:");
      Multiple_Flag_Utilities.Help_On_Flags (Header => "Parameter(2..): [flexible]",
                                             Footer => "(default = all)");
      User_Message;
      User_Message ("For no_closing_name:");
      User_Message ("Parameter(2): maximum number of lines allowed");
      User_Message;
      User_Message ("For numeric_literal:");
      User_Message ("Parameter(2): [not] <base>");
      User_Message ("Parameter(3): <block_size>");
      User_Message;
      User_Message ("For parameter_order:");
      Extended_Modes_Utilities.Help_On_Modifiers (Header => "parameter(3..): list of",
                                                  Footer => "(separated by '|')");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control(Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Asis;
      use Framework.Language, Ada.Strings.Wide_Unbounded;
      use Casing_Flag_Utilities, Literal_Flag_Utilities, Multiple_Flag_Utilities;
      use Place_Flag_Utilities, Subrules_Flag_Utilities, Parameter_Ordering_Machine;

      Subrule  : Subrules;
      Max      : ASIS_Integer;
      Multiple : Multiple_Names;
      Lit_Kind : Literal_Names;
      Places   : Place_Set := (others => False);
      P        : Place_Names;
      Flexible : Boolean;
      Is_Max   : Boolean;
   begin
      if Parameter_Exists then
         Subrule := Get_Flag_Parameter (Allow_Any => False);
         case Subrule is
            when St_Compound_Statement
              | St_Default_In
              | St_Renamed_Entity
              =>
               -- Those without parameters
               if Parameter_Exists then
                  Parameter_Error (Rule_Id, "subrule """ & Image (Subrule, Lower_Case) & """ has no parameter");
               end if;
               Associate (Contexts, Value (Image (Subrule, Lower_Case)), Basic.New_Context (Ctl_Kind, Ctl_Label));
               Rule_Used (Subrule) := True;

            when Casing_Styles =>
               Associate (Contexts, Value (Image (Subrule, Lower_Case)), Basic.New_Context (Ctl_Kind, Ctl_Label));
               if Parameter_Exists then
                  loop
                     Casing_Policy (Subrule) (Get_Flag_Parameter (Allow_Any => False)) := True;
                     if Casing_Policy (Subrule) (Ca_Original) and Subrule /= St_Casing_Identifier then
                        Parameter_Error (Rule_Id, """Original"" allowed only for identifiers");
                     end if;
                     if Subrule in St_Casing_Exponent | St_Casing_Number
                       and Casing_Policy (Subrule) (Ca_Titlecase)
                     then
                        Parameter_Error (Rule_Id, """Titlecase"" not allowed for exponents and numbers");
                     end if;
                     exit when not Parameter_Exists;
                  end loop;
               else
                  Parameter_Error (Rule_Id, "missing indication of casing policy");
               end if;
               Rule_Used (Subrule) := True;

            when St_Exposed_Literal =>
               if Parameter_Exists then
                  Lit_Kind := Get_Flag_Parameter (Allow_Any => False);
               else
                  Parameter_Error (Rule_Id, "missing kind of literal for Exposed_Literal");
               end if;

               while Parameter_Exists loop
                  P := Get_Flag_Parameter (Allow_Any => True);
                  if P = Pl_Other then
                     -- It is an allowed value

                     Is_Max := Get_Modifier ("MAX");
                     case Lit_Kind is
                        when Lit_Integer =>
                           if Is_Max then
                              if Integer_Max_Value /= Uninitialized then
                                 Parameter_Error (Rule_Id, "max value already given");
                              end if;

                              Integer_Max_Value := Get_Integer_Parameter;
                              if Integer_Max_Value < 0 then
                                 Integer_Max_Value := Uninitialized;   -- Reinitialized to uninitialized ;-)
                                 Parameter_Error (Rule_Id, "max value cannot be negative");
                              end if;
                           else
                              if Integer_Count = Permitted_Consts_Count'Last then
                                 Parameter_Error (Rule_Id, "too many integer values");
                              end if;

                              Integer_Count := Integer_Count + 1;
                              Integer_Permitted_Values (Integer_Count) := Get_Integer_Parameter;
                           end if;
                        when Lit_Real =>
                           if Is_Max then
                              Parameter_Error (Rule_Id, "max not allowed for reals");
                           end if;

                           if Real_Count = Permitted_Consts_Count'Last then
                              Parameter_Error (Rule_Id, "too many real values");
                           end if;

                           Real_Count := Real_Count + 1;
                           Real_Permitted_Values (Real_Count) := Get_Float_Parameter;
                        when Lit_String =>
                           if Is_Max then
                              Parameter_Error (Rule_Id, "max not allowed for strings");
                           end if;

                           if String_Count = Permitted_Consts_Count'Last then
                              Parameter_Error (Rule_Id, "too many string values");
                           end if;

                           String_Count := String_Count + 1;
                           String_Permitted_Values (String_Count) := To_Unbounded_Wide_String (Get_String_Parameter);
                        when Lit_Character =>
                           Parameter_Error (Rule_Id, "no exceptions allowed for character literals");
                     end case;
                  else
                     -- it is a Place keyword
                     Places (P) := True;
                  end if;
               end loop;

               if Places = No_Place then
                  -- No Place specified, default to Constant + Number
                  Places := (Pl_Constant | Pl_Number => True, others => False);
               end if;
               Permitted_Places (Lit_Kind) := Places;

               Associate (Contexts,
                          Value (Image (St_Exposed_Literal, Lower_Case) & Image (Lit_Kind, Lower_Case)),
                          Basic.New_Context (Ctl_Kind, Ctl_Label));
               Rule_Used (St_Exposed_Literal) := True;

            when St_Multiple_Elements =>
               if Parameter_Exists then
                  while Parameter_Exists loop
                     Flexible := Get_Modifier ("FLEXIBLE");
                     Multiple := Get_Flag_Parameter (Allow_Any => False);

                     if Multiple = Mu_Clause then
                        Flexible_Clause := Flexible;
                     elsif Flexible then
                        Parameter_Error (Rule_Id, """flexible"" allowed only for ""clause""");
                     end if;

                     if Multiple = Mu_Keywords then
                        for K in Multiple_Keywords loop
                           Associate (Contexts,
                                      Value (Image (St_Multiple_Elements, Lower_Case) & Image (K)),
                                      Basic.New_Context (Ctl_Kind, Ctl_Label));
                        end loop;
                     else
                        Associate (Contexts,
                                   Value (Image (St_Multiple_Elements, Lower_Case) & Image (Multiple)),
                                   Basic.New_Context (Ctl_Kind, Ctl_Label));
                     end if;
                  end loop;
               else
                  for M in Multiple_Names loop
                     Associate (Contexts,
                                Value (Image (St_Multiple_Elements, Lower_Case) & Image (M, Lower_Case)),
                                Basic.New_Context (Ctl_Kind, Ctl_Label));
                  end loop;
               end if;
               Rule_Used (St_Multiple_Elements) := True;

            when St_No_Closing_Name =>
               if Parameter_Exists then
                  Max := Get_Integer_Parameter (Min => 0);
               else
                  Max := -1;
               end if;

               Associate (Contexts,
                          Value (Image (St_No_Closing_Name, Lower_Case) & Control_Kinds'Wide_Image (Ctl_Kind)),
                          Closing_Name_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Max));
               Rule_Used (St_No_Closing_Name) := True;

            when St_Numeric_Literal =>
               if not Parameter_Exists then
                  Parameter_Error (Rule_Id, "parameter required for Numeric_Literal");
               end if;

               declare
                  -- Values needed for contexts
                  Is_Not     : constant Boolean := Get_Modifier ("NOT");
                  Base       : Allowed_Bases;
                  Block_Size : Asis.ASIS_Natural;
                  -- Validation buffer
               begin
                  -- Retrieve the user-defined base
                  Base := Get_Integer_Parameter (Min => Allowed_Bases'First, Max => Allowed_Bases'Last);

                  -- Check for next parameters
                  if Is_Not then
                     if Parameter_Exists then
                        Parameter_Error (Rule_Id, "cannot specify block size for unauthorized base");
                     end if;
                     Block_Size := 0;
                  else
                     if not Parameter_Exists then
                        Parameter_Error (Rule_Id, "block size needed for base");
                     end if;
                     Block_Size := Get_Integer_Parameter (Min => 0);
                  end if;

                  Associate (Contexts,
                             Value (Image (St_Numeric_Literal, Lower_Case) & Allowed_Bases'Wide_Image (Base)),
                             Literal_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Is_Not, Block_Size));
                  Rule_Used (St_Numeric_Literal) := True;
              end;

            when St_Parameter_Order | St_Formal_Parameter_Order =>
               declare
                  use Extended_Modes_Utilities;
                  State         : Modifier_Set;
                  Not_Specified : Modifier_Set := Full_Set;
               begin
                  if Rule_Used (Subrule) then
                     Parameter_Error (Rule_Id, Image (Subrule, Lower_Case) & " already specified for rule");
                  end if;
                  if Parameter_Exists then
                     loop
                        State         := Get_Modifier_Set (No_Parameter => True);
                        Not_Specified := Not_Specified and not State;
                        case St_Orders (Subrule) is
                           when St_Parameter_Order =>
                              Add_State (Parameter_Ordering, State);
                           when St_Formal_Parameter_Order =>
                              Add_State (Formal_Parameter_Ordering, State);
                        end case;
                        exit when not Parameter_Exists;
                     end loop;
                     if Not_Specified /= Empty_Set then
                        -- allow all modes not explicitely specified after the ones specified
                        case St_Orders (Subrule) is
                           when St_Parameter_Order =>
                              Add_State (Parameter_Ordering, Not_Specified);
                           when St_Formal_Parameter_Order =>
                              Add_State (Formal_Parameter_Ordering, Not_Specified);
                        end case;
                     end if;
                  else
                     case St_Orders (Subrule) is
                        when St_Parameter_Order =>
                           Add_State (Parameter_Ordering, (Mode_In | Mode_Access => True, others => False));
                           Add_State (Parameter_Ordering, (Mode_In_Out           => True, others => False));
                           Add_State (Parameter_Ordering, (Mode_Out              => True, others => False));
                           Add_State (Parameter_Ordering, (Mode_Defaulted_In     => True, others => False));
                        when St_Formal_Parameter_Order =>
                           Add_State (Formal_Parameter_Ordering,
                                      (Mode_Type                                 => True, others => False));
                           Add_State (Formal_Parameter_Ordering,
                                      (Mode_In | Mode_Access | Mode_Defaulted_In => True, others => False));
                           Add_State (Formal_Parameter_Ordering,
                                      (Mode_In_Out                               => True, others => False));
                           Add_State (Formal_Parameter_Ordering,
                                      (Mode_Procedure | Mode_Function            => True, others => False));
                           Add_State (Formal_Parameter_Ordering,
                                      (Mode_Package                              => True, others => False));
                     end case;
                  end if;
               end;
               Rule_Used (Subrule) := True;
               Associate (Contexts, Value (Image (Subrule, Lower_Case)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         end case;

      else
         -- No parameter => all style checks
         Rule_Used := (others => True);

         -- Casing_Aspect
         Associate (Contexts, Value (Image (St_Casing_Aspect)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Aspect) := (Ca_Titlecase => True, others => False);

         -- Casing_Attribute
         Associate (Contexts, Value (Image (St_Casing_Attribute)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Attribute) := (Ca_Titlecase => True, others => False);

         -- Casing_Exponent
         Associate (Contexts, Value (Image (St_Casing_Exponent)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Exponent) := (Ca_Uppercase => True, others => False);

         -- Casing_Identifier
         Associate (Contexts, Value (Image (St_Casing_Identifier)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Identifier) := (Ca_Original => True, others => False);

         -- Casing_Keyword
         Associate (Contexts, Value (Image (St_Casing_Keyword)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Keyword) := (Ca_Lowercase => True, others => False);

         -- Casing_Number
         Associate (Contexts, Value (Image (St_Casing_Number)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Number) := (Ca_Uppercase => True, others => False);

         -- Casing_Pragma
         Associate (Contexts, Value (Image (St_Casing_Pragma)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Pragma) := (Ca_Titlecase => True, others => False);

         -- Compound_Statement
         Associate (Contexts, Value (Image (St_Compound_Statement)), Basic.New_Context (Ctl_Kind, Ctl_Label));

         -- Default_In
         Associate (Contexts, Value (Image (St_Default_In)), Basic.New_Context (Ctl_Kind, Ctl_Label));

         -- Exposed_Literal
         Associate (Contexts,
                    Value (Image (St_Exposed_Literal) & Image (Lit_Integer)),
                    Basic.New_Context (Ctl_Kind, Ctl_Label));
         Integer_Count            := 2;
         Integer_Permitted_Values := (0, 1, others=> 0);

         Associate (Contexts,
                    Value (Image (St_Exposed_Literal) & Image (Lit_Real)),
                    Basic.New_Context (Ctl_Kind, Ctl_Label));
         Real_Count            := 2;
         Real_Permitted_Values := (0.0, 1.0, others => 0.0);
         Permitted_Places      := (others => (Pl_Constant | Pl_Number => True, others => False));

         -- Multiple_Elements
         for M in Multiple_Names loop
            Associate (Contexts,
                       Value (Image (St_Multiple_Elements) & Image (M)),
                       Basic.New_Context (Ctl_Kind, Ctl_Label));
         end loop;

         -- No_Closing_Name
         Associate (Contexts,
                    Value (Image (St_No_Closing_Name) & Control_Kinds'Wide_Image (Ctl_Kind)),
                    Closing_Name_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Length => -1));

         -- Numeric_Literal
         Associate (Contexts,
                    Value (Image (St_Numeric_Literal) & Allowed_Bases'Wide_Image (10)),
                    Literal_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label)
                                                 with Is_Not => False, Block_Size => 3));

         -- Parameter_Order
         Associate (Contexts, Value (Image (St_Parameter_Order)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Add_State (Parameter_Ordering, (Mode_In | Mode_Access => True, others => False));
         Add_State (Parameter_Ordering, (Mode_In_Out           => True, others => False));
         Add_State (Parameter_Ordering, (Mode_Out              => True, others => False));
         Add_State (Parameter_Ordering, (Mode_Defaulted_In     => True, others => False));

         -- Formal_Parameter_Order
         Associate (Contexts, Value (Image (St_Formal_Parameter_Order)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Add_State (Formal_Parameter_Ordering, (Mode_Type                                 => True, others => False));
         Add_State (Formal_Parameter_Ordering, (Mode_In | Mode_Access | Mode_Defaulted_In => True, others => False));
         Add_State (Formal_Parameter_Ordering, (Mode_In_Out                               => True, others => False));
         Add_State (Formal_Parameter_Ordering, (Mode_Procedure | Mode_Function            => True, others => False));
         Add_State (Formal_Parameter_Ordering, (Mode_Package                              => True, others => False));
       end if;
   exception
      when Already_In_Store =>
         Parameter_Error (Rule_Id, "Subrule already provided: " & Image (Subrule, Lower_Case));
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager, Parameter_Ordering_Machine;
   begin
      case Action is
         when Clear =>
            Clear (Contexts);
            Rule_Used         := (others => False);
            Real_Count        := 0;
            Integer_Count     := 0;
            Integer_Max_Value := Uninitialized;
            String_Count      := 0;
            Casing_Policy     := (others => (others => False));
            Permitted_Places  := (others => (others => False));
            Flexible_Clause   := False;
            Reset (Parameter_Ordering);
            Reset (Formal_Parameter_Ordering);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      -- Make defaults for unspecified Check/Search/Count for style No_Closing_Name
      use Subrules_Flag_Utilities;
   begin
      if Rule_Used (St_Renamed_Entity) then
         Renamed_Entities.Activate;
      end if;

      if Rule_Used (St_No_Closing_Name) then
         for R in Control_Kinds loop
            if Corresponding_Context (St_No_Closing_Name, Control_Kinds'Wide_Image (R)) = No_Matching_Context then
               Associate (Contexts,
                          Value (Image (St_No_Closing_Name) & Control_Kinds'Wide_Image (R)),
                          Closing_Name_Context'(Basic.New_Context (R, "") with Asis.ASIS_Integer'Last));
            end if;
         end loop;
      end if;
   end Prepare;

   -----------------
   -- Is_Same_Def --
   -----------------

   function Is_Same_Def (L, R : Renaming_Data) return Boolean is
      use Asis.Elements;
   begin
      return Is_Equal (L.Renamed_Def, R.Renamed_Def);
   end Is_Same_Def;

   ---------------------------
   -- Corresponding_Context --
   ---------------------------

   function Corresponding_Context (Subrule : Subrules; Complement : Wide_String := "") return Root_Context'Class is
      use Subrules_Flag_Utilities;
   begin
      return Association (Contexts, Image (Subrule) & Complement);
   end Corresponding_Context;

   ---------------
   -- Should_Be --
   ---------------

   function Should_Be (Source   : Wide_String;
                       Expected : Casing_Set;
                       Original : Wide_String := "";
                       For_Fix  : Boolean) return Wide_String
   is
      use Ada.Strings.Wide_Unbounded;

      Result : Unbounded_Wide_String;
      procedure Append_Result (Name : Wide_String; Force : Boolean := False) is
      begin
         if not Force and then Expected (Ca_Original) and then Name = Original then
            -- Don't issue the name twice if the original is the same as another allowed one
            return;
         end if;
         if Result /= Null_Unbounded_Wide_String then
            Append (Result, " or ");
         end if;
         Append (Result, '"' & Name & '"');
      end Append_Result;
   begin   --  Should_Be
      for E in reverse Expected'Range loop  -- reverse to favour Ca_Original over the other ones
         if Expected (E) then
            case E is
               when Ca_Original =>
                  if Original /= (Original'Range => ' ') then  -- works with ""
                     if For_Fix then
                        return Original;
                     end if;
                     Append_Result (Original, Force => True);
                  end if;
               when Ca_Uppercase =>
                  if For_Fix then
                     return To_Upper (Source);
                  end if;
                  Append_Result (To_Upper (Source));
               when Ca_Lowercase =>
                  if For_Fix then
                     return To_Lower (Source);
                  end if;
                  Append_Result (To_Lower (Source));
               when Ca_Titlecase =>
                  if For_Fix then
                     return To_Title (Source);
                  end if;
                  Append_Result (To_Title (Source));
            end case;
         end if;
      end loop;

      return To_Wide_String (Result);
   end Should_Be;

   ------------------
   -- Check_Casing --
   ------------------

   procedure Check_Casing (Casing         : in Casing_Styles;
                           Source_Element : in Asis.Element;
                           Ref_Element    : in Asis.Element := Asis.Nil_Element)
   is
   -- Source_Element is the identifier for St_Casing_Identifier and St_Casing_Attribute
   -- and the pragma for St_Casing_Pragma
   -- Ref_Element is an element that allows retrieving the original defining name. If it is Nil_Element,
   -- Source_Element is used instead. It differs from Source_Element only for end names.
      use Asis, Asis.Declarations, Asis.Elements;
      use Framework.Locations, Framework.Reports, Thick_Queries;

      Source_Image   : constant Wide_String := Extended_Name_Image (Source_Element);
      Original_Image : Wide_String (Source_Image'Range) := (others => ' ');
      -- Note that the source name and the original name always have the same length!
      Def_Name       : Asis.Defining_Name;
   begin
      for Name in Casing_Names loop
         if Casing_Policy (Casing) (Name) then
            case Name is
               when Ca_Uppercase =>
                  if Source_Image =  To_Upper (Source_Image) then
                     return;
                  end if;
               when Ca_Lowercase =>
                  if Source_Image =  To_Lower (Source_Image) then
                     return;
                  end if;
               when Ca_Titlecase =>
                  if Source_Image =  To_Title (Source_Image) then
                     return;
                  end if;
               when Ca_Original =>
                  declare
                     Good_Ref : Asis.Element := Ref_Element;
                  begin
                     if Is_Nil (Good_Ref) then
                        Good_Ref := Source_Element;
                     end if;
                     Def_Name := First_Defining_Name (Good_Ref);

                     if (Element_Kind (Good_Ref) = A_Defining_Name and then Is_Equal (Source_Element, Def_Name))
                       -- Don't check when it *is* the original...
                       or Is_Nil (Def_Name)
                       -- some predefined stuff, give up
                     then
                        -- Note that we assume here that there are no other policy after Ca_Original
                        return;
                     end if;

                     if Defining_Name_Kind (Def_Name) = A_Defining_Expanded_Name then
                        Def_Name := Defining_Selector (Def_Name);
                     end if;
                     Original_Image := Defining_Name_Image (Def_Name);
                     if Source_Image = Original_Image then
                        return;
                     end if;
                  end;
            end case;
         end if;
      end loop;

      if Element_Kind (Source_Element) = A_Pragma then
         -- The pragma name cannot be accessed as an element, hence special handling
         declare
            Pragma_Loc : constant Location := Get_Next_Word_Location (Source_Element,
                                                                      Starting => From_Head,
                                                                      Skipping => 1);
         begin
            Report (Rule_Id,
                    Corresponding_Context (Casing),
                    Pragma_Loc,
                    "Wrong casing of """ & Source_Image
                    & """, should be "
                    & Should_Be (Source_Image, Casing_Policy (Casing), For_Fix => False));
            Fixes.Replace (Pragma_Loc,
                           Source_Image'Length,
                           By => Should_Be (Source_Image, Casing_Policy (Casing), For_Fix => True));
         end;
      else
         Report (Rule_Id,
                 Corresponding_Context (Casing),
                 Get_Location (Source_Element),
                 "Wrong casing of """ & Source_Image
                 & """, should be "
                 & Should_Be (Source_Image,  Casing_Policy (Casing), Original_Image, For_Fix => False));

         Fixes.Replace (Source_Element,
                        Should_Be (Source_Image, Casing_Policy (Casing), Original_Image, For_Fix => True));
      end if;
   end Check_Casing;

   -----------------------
   -- Process_Construct --
   -----------------------

   -- Controls declarations that allow repeating the name at the end of the declaration, and where the closing name
   -- is omitted

   procedure Process_Construct (Construct : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Text;
      use Framework.Locations, Framework.Reports;
      Length : Line_Number;
   begin
      if not Rule_Used (St_No_Closing_Name) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Construct) is
         when A_Null_Procedure_Declaration =>
            -- Syntax does not allow a closing name
            return;
         when A_Task_Type_Declaration =>
            if Is_Nil (Type_Declaration_View (Construct)) then
               -- No task definition => no closing name possible
               return;
            end if;
         when A_Single_Task_Declaration =>
            if Is_Nil (Object_Declaration_View (Construct)) then
               -- No task definition => no closing name possible
               return;
            end if;
         when others =>
            null;
      end case;

      if not Is_Name_Repeated (Construct) then
         Length := A4G_Bugs.Last_Line_Number (Construct) - A4G_Bugs.First_Line_Number (Construct) + 1;
         if Length > Closing_Name_Context (Corresponding_Context (St_No_Closing_Name,
                                                                  Control_Kinds'Wide_Image (Check))).Length
         then
            Report (Rule_Id,
                    Corresponding_Context (St_No_Closing_Name, Control_Kinds'Wide_Image (Check)),
                    Get_Location (Construct),
                    "name not repeated at the end");
         elsif Length > Closing_Name_Context (Corresponding_Context (St_No_Closing_Name,
                                                                     Control_Kinds'Wide_Image (Search))).Length
         then
            Report (Rule_Id,
                    Corresponding_Context (St_No_Closing_Name, Control_Kinds'Wide_Image (Search)),
                    Get_Location (Construct),
                    "name not repeated at the end");
         end if;
         if Length > Closing_Name_Context (Corresponding_Context (St_No_Closing_Name,
                                                                  Control_Kinds'Wide_Image (Count))).Length
         then
            Report (Rule_Id,
                    Corresponding_Context (St_No_Closing_Name, Control_Kinds'Wide_Image (Count)),
                    Get_Location (Construct),
                    "name not repeated at the end");
         end if;

         Fixes.Insert (' ' & Defining_Name_Image (Names (Construct) (1)),
                      From => Get_Previous_Word_Location (Construct, Matching => "END", Starting => From_Tail) + 3);
      end if;

   end Process_Construct;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Identifier : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions;

      procedure Check_Renamed is
         use Asis.Declarations;
         use Scope_Manager, Framework.Locations, Framework.Reports;

         Def : constant Asis.Definition := Corresponding_Name_Definition (Identifier);
         Ren : Renaming_Data;
      begin
         if Is_Nil (Def) then
            -- Predefined stuff
            return;
         end if;

         -- Comparison only on Renamed_Definition:
         Renamed_Entities.Reset ((Null_Location, Nil_Element, Def), All_Scopes);
         if not Renamed_Entities.Data_Available then
            return;
         end if;

         Ren := Renamed_Entities.Current_Data;
         Report (Rule_Id,
                 Corresponding_Context (St_Renamed_Entity),
                 Get_Location (Identifier),
                 Defining_Name_Image (Def) & " has been renamed at " & Image (Ren.Ren_Location));
         Fixes.Replace (Identifier, By => Defining_Name_Image (Ren.Renaming_Def));
      end Check_Renamed;

      procedure Check_End_Casing (Casing : Subrules) is
      -- Check casing of name after "end"
      -- Called only for defining identifiers, or the name of an accept statement
         use Asis.Declarations, Asis.Statements;

         Encl_Decl : Asis.Element := Identifier;
         Decl_Name : Asis.Element;
         End_Name  : Asis.Element;
      begin
         -- Find the declaration (or statement) that encloses the [defining] name
         loop
            case Element_Kind (Encl_Decl) is
               when A_Defining_Name =>
                  -- we are still inside an expanded defining name
                  Encl_Decl := Enclosing_Element (Encl_Decl);
               when An_Expression =>
                  -- happens only for the identifier of an accept statement
                  Encl_Decl := Enclosing_Element (Encl_Decl);
               when others =>
                  exit;
            end case;
         end loop;
         case Element_Kind (Encl_Decl) is
            when A_Declaration =>
               case Declaration_Kind (Encl_Decl) is
                  when A_Package_Declaration
                     | A_Package_Body_Declaration
                     | A_Procedure_Body_Declaration
                     | A_Function_Body_Declaration
                     | A_Generic_Package_Declaration
                     | A_Task_Type_Declaration
                     | A_Single_Task_Declaration
                     | A_Task_Body_Declaration
                     | A_Protected_Type_Declaration
                     | A_Single_Protected_Declaration
                     | A_Protected_Body_Declaration
                     | An_Entry_Body_Declaration
                     =>
                     null;
                  when others =>
                     return;
               end case;
            when A_Statement =>
               case Statement_Kind (Encl_Decl) is
                  when A_Loop_Statement
                     | A_While_Loop_Statement
                     | A_For_Loop_Statement
                     | A_Block_Statement
                     | An_Accept_Statement
                     =>
                     null;
                  when others =>
                     return;
               end case;
            when others =>
               return;
         end case;
         End_Name := Corresponding_End_Name (Encl_Decl);
         if Is_Nil (End_Name) then
            return;
         end if;

         -- Here we have an end name to check
         -- We must get the defining name from the true name in the declaration, since
         -- it is impossible to get it from the end name
         case Element_Kind (Encl_Decl) is
            when A_Declaration =>
               Decl_Name := Names (Encl_Decl) (1);
            when A_Statement =>
               if Statement_Kind (Encl_Decl) = An_Accept_Statement then
                  Decl_Name := Accept_Entry_Direct_Name (Encl_Decl);
               else
                  Decl_Name := Statement_Identifier (Encl_Decl);
               end if;
            when others =>
               Failure ("Wrong enclosing of identifier", Encl_Decl);
         end case;
         if Expression_Kind (End_Name) = A_Selected_Component then
            -- Note that the rightmost name of Decl_Name is a defining name, but all others
            -- are identifiers
            Check_Casing (Casing,
                          Source_Element =>          Selector (End_Name),
                          Ref_Element    => Defining_Selector (Decl_Name));
            End_Name  :=          Prefix (End_Name);
            Decl_Name := Defining_Prefix (Decl_Name);
            while Expression_Kind (End_Name) = A_Selected_Component loop
               Check_Casing (Casing,
                             Source_Element => Selector (End_Name),
                             Ref_Element    => Selector (Decl_Name));
               End_Name  := Prefix (End_Name);
               Decl_Name := Prefix (Decl_Name);
            end loop;
         end if;
         Check_Casing (Casing,
                       Source_Element => End_Name,
                       Ref_Element    => Decl_Name);
      end Check_End_Casing;

   begin  -- Process_Identifier
      if not (Rule_Used (St_Casing_Identifier)
              or Rule_Used (St_Renamed_Entity)
              or Rule_Used (St_Casing_Keyword))
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Beware that if Identifier is A_Defining_Operator_Symbol or An_Operator_Symbol, we must
      -- apply the rule for keywords, not identifiers
      if Element_Kind (Identifier) = A_Defining_Name then
         if Rule_Used (St_Casing_Identifier)
           and then (        Defining_Name_Kind (Identifier) = A_Defining_Identifier
                     or else Defining_Name_Kind (Identifier) = A_Defining_Enumeration_Literal)
         then
            Check_Casing (St_Casing_Identifier, Identifier);
            Check_End_Casing (St_Casing_Identifier);
         elsif Rule_Used (St_Casing_Keyword) and then Defining_Name_Kind (Identifier) = A_Defining_Operator_Symbol then
            Check_Casing (St_Casing_Keyword, Identifier);
            Check_End_Casing (St_Casing_Keyword);
         end if;
         -- No need to call Check_Renamed on defining names

      else
         if Rule_Used (St_Casing_Identifier)
           and then (        Expression_Kind (Identifier) = An_Identifier
                     or else Expression_Kind (Identifier) = An_Enumeration_Literal)
         then
            Check_Casing (St_Casing_Identifier, Identifier);
            if Statement_Kind (Enclosing_Element (Identifier)) = An_Accept_Statement then
               -- Since E in "accept E" is an identifier, not a defining_name, we must check the end name here
               -- Lucky that an entry cannot be an operator symbol!
               Check_End_Casing (St_Casing_Identifier);
            end if;
         elsif Rule_Used (St_Casing_Keyword) and then Expression_Kind (Identifier) = An_Operator_Symbol then
            -- This is an operator, must be the prefix of a function call
            -- If it uses infix notation, don't handle it because it will be found by the texual rule for keywords
            if Is_Prefix_Call (Enclosing_Element (Identifier)) then
               Check_Casing (St_Casing_Keyword, Identifier);
            end if;
         end if;

         if Rule_Used (St_Renamed_Entity) then
            Check_Renamed;
         end if;
      end if;

   end Process_Identifier;

   --------------------------------
   -- Process_Compound_Statement --
   --------------------------------

   procedure Process_Compound_Statement (Statement : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements, Asis.Text;
      use Framework.Locations, Framework.Reports, Thick_Queries;
      Kind      : Asis.Statement_Kinds;
   begin
      if not Rule_Used (St_Compound_Statement) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Kind := Statement_Kind (Statement);

      -- Special case for accept statement: check only if there is a body
      if Kind = An_Accept_Statement and then Is_Nil (Accept_Body_Statements (Statement)) then
         return;
      end if;

      if Lines_Span_Length (Statement) < Min_Stat_Length (Kind) then
         Report (Rule_Id,
                 Corresponding_Context (St_Compound_Statement),
                 Get_Location (Statement),
                 "Statement has less than" & Line_Number'Wide_Image (Min_Stat_Length (Kind)) & " lines");
      end if;
   end Process_Compound_Statement;

   -------------------------
   -- Process_Declaration --
   -------------------------

   -- Checking that all mode parameter are explicit (no default in) or in order
   --
   -- It just uses the Mode_Kind function provides by ASIS
   -- on parameter specification obtained from SP or generics declarations

   procedure Process_Declaration (Declaration: in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;

      -------------------
      -- Check_Formals --
      -------------------

      procedure Check_Formals (Subrule : St_Orders; Formals : Asis.Element_List) is
         use Framework.Locations, Framework.Reports, Parameter_Ordering_Machine;
         use all type Framework.Reports.Fixes.Insert_Place;

         Enclosing : Asis.Declaration;
         E_Mode    : Extended_Modes;
         Ignore    : Boolean;

         function Object_Mode (Formal : Asis.Declaration) return Extended_Modes is
         begin
            case Mode_Kind (Formal) is
               when Not_A_Mode =>
                  Failure ("style: not_a_mode in process_declaration");
               when An_In_Mode =>
                  if Is_Nil (Initialization_Expression (Formal)) then
                     return Mode_In;
                  else
                     return Mode_Defaulted_In;
                  end if;
               when A_Default_In_Mode =>
                  if Definition_Kind (Object_Declaration_View (Formal)) = An_Access_Definition then
                     return Mode_Access;
                  elsif Is_Nil (Initialization_Expression (Formal)) then
                     return Mode_In;
                  else
                     return Mode_Defaulted_In;
                  end if;
               when An_Out_Mode =>
                  return Mode_Out;
               when An_In_Out_Mode =>
                  return Mode_In_Out;
            end case;
         end Object_Mode;

      begin  -- Check_Formals
         if Formals = Nil_Element_List then
            return;
         end if;

         if Rule_Used (St_Default_In) then
            for F : Asis.Element of Formals loop
               -- Note: Mode_Kind returns Not_A_Mode for generic formals that are not objects (types, subprograms...),
               --  so we don't need to special case these
               if Mode_Kind (F) = A_Default_In_Mode
                 and then Definition_Kind (Object_Declaration_View (F)) /= An_Access_Definition
               then
                  Report (Rule_Id,
                          Corresponding_Context (St_Default_In),
                          Get_Location (Declaration_Subtype_Mark (F)),
                          "default IN mode used for parameter");
                  Fixes.Insert ("in ", Before, Declaration_Subtype_Mark (F));
               end if;
            end loop;
         end if;

         if Subrule = St_Parameter_Order and Rule_Used (St_Parameter_Order) then
            Enclosing := Enclosing_Element (Formals (Formals'First));
           -- if it is a subprogram with an explicit spec, do not repeat message on body
            if Declaration_Kind (Enclosing) not in A_Procedure_Body_Declaration .. A_Function_Body_Declaration
                or else Is_Nil (Corresponding_Declaration (Enclosing))
            then
               Set_Initial (Parameter_Ordering);
               for F : Asis.Element of Formals loop
                  Set_State (Parameter_Ordering, Object_Mode (F));
                  if not Is_Allowed (Parameter_Ordering) then
                     Report (Rule_Id,
                             Corresponding_Context (Subrule),
                             Get_Location (F),
                             "subprogram parameter out of order");
                  end if;
               end loop;
            end if;
         end if;

         if Subrule = St_Formal_Parameter_Order and Rule_Used (St_Formal_Parameter_Order) then
            Set_Initial (Formal_Parameter_Ordering);
            for F : Asis.Element of Formals loop
               Ignore := False;
               case Declaration_Kind (F) is
                  when A_Formal_Object_Declaration =>
                     E_Mode  := Object_Mode (F);
                  when A_Formal_Type_Declaration | A_Formal_Incomplete_Type_Declaration =>
                     E_Mode  := Mode_Type;
                  when A_Formal_Procedure_Declaration =>
                     E_Mode  := Mode_Procedure;
                  when A_Formal_Function_Declaration =>
                     E_Mode  := Mode_Function;
                  when A_Formal_Package_Declaration
                     | A_Formal_Package_Declaration_With_Box
                     =>
                     E_Mode  := Mode_Package;
                  when Not_A_Declaration =>
                     -- presumably, a use clause for a generic formal package
                     Ignore := True;
                  when others =>
                     Failure ("style: inappropriate declaration_kind for formal", F);
               end case;

               if not Ignore then
                  Set_State (Formal_Parameter_Ordering, E_Mode);
                  if not Is_Allowed (Formal_Parameter_Ordering) then
                     Report (Rule_Id,
                             Corresponding_Context (Subrule),
                             Get_Location (F),
                             "generic formal parameter out of order");
                  end if;
               end if;
            end loop;
         end if;
      end Check_Formals;

   begin  -- Process_Declaration
      if not Rule_Used (St_Default_In)
        and not Rule_Used (St_Parameter_Order)
        and not Rule_Used (St_Formal_Parameter_Order)
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Declaration) is
         when A_Procedure_Declaration
            | A_Null_Procedure_Declaration
            | A_Function_Declaration
            | An_Expression_Function_Declaration   -- Ada 2012
            | A_Procedure_Body_Declaration
            | A_Function_Body_Declaration
            | A_Procedure_Renaming_Declaration
            | A_Function_Renaming_Declaration
            | An_Entry_Declaration
            | An_Entry_Body_Declaration
            | A_Procedure_Body_Stub
            | A_Function_Body_Stub
            | A_Formal_Function_Declaration
            | A_Formal_Procedure_Declaration
            =>
            Check_Formals (St_Parameter_Order, Parameter_Profile (Declaration));

         when A_Generic_Procedure_Declaration
           | A_Generic_Function_Declaration
              =>
            Check_Formals (St_Formal_Parameter_Order, Generic_Formal_Part (Declaration));
            Check_Formals (St_Parameter_Order, Parameter_Profile (Declaration));

          when A_Generic_Package_Declaration =>
            Check_Formals (St_Formal_Parameter_Order, Generic_Formal_Part (Declaration));

         when others =>
            null;
      end case;
   end Process_Declaration;


   ---------------------
   -- Process_Literal --
   ---------------------

   procedure Process_Literal (Expression : in Asis.Expression) is
      use Asis.Expressions, Asis.Text;

      -- Retrieve indices of main number formatters
      procedure Number_Decomposition (Name             : in     Wide_String;
                                      Base_Delimiter_1 :    out Character_Position;
                                      Base_Delimiter_2 :    out Character_Position;
                                      Dot_Delimiter    :    out Character_Position;
                                      Exp_Delimiter    :    out Character_Position)
      is
         Is_Based_Literal     : Boolean := False;
         Within_Based_Literal : Boolean := False;
         Is_Real_Literal      : Boolean := False;
         Is_Exponentiated     : Boolean := False;
      begin
         Base_Delimiter_1 := 0;
         Base_Delimiter_2 := 0;
         Dot_Delimiter    := 0;
         Exp_Delimiter    := 0;
         -- Retrieve true indices
         for Idx in Name'Range loop
            if Name (Idx) = '#' then
               if Is_Based_Literal then
                  Base_Delimiter_2 := Character_Position (Idx);
               else
                  Base_Delimiter_1 := Character_Position (Idx);
                  Is_Based_Literal := True;
               end if;
               Within_Based_Literal := not Within_Based_Literal;
            elsif Name (Idx) = '.' then
               Is_Real_Literal := True;
               Dot_Delimiter   := Character_Position (Idx);
            elsif (Name (Idx) = 'e' or else Name (Idx) = 'E') and then not Within_Based_Literal then
               Is_Exponentiated := True;
               Exp_Delimiter    := Character_Position (Idx);
            end if;
         end loop;
         -- Adjust values according to the kind of literal
         if not Is_Exponentiated then
            Exp_Delimiter := Character_Position (Name'Last) + 1;
         end if;
         if not Is_Based_Literal then
            Base_Delimiter_1 := Character_Position (Name'First) - 1;
            Base_Delimiter_2 := Exp_Delimiter;
         end if;
         if not Is_Real_Literal then
            Dot_Delimiter := Base_Delimiter_2;
         end if;
      end Number_Decomposition;


      -- Check separator positions according to the convention
      function Check_Separators (Name : in Wide_String; Block_Size : in Asis.ASIS_Positive) return Boolean is
         use type Asis.ASIS_Integer;

         Step   : constant Asis.Text.Character_Position := Block_Size + 1;  -- The base step for separators
         Cursor : Asis.Text.Character_Position          := 1;               -- The current tested character position
      begin
         for C : Wide_Character of Name loop
            -- Check if we should, or should not, match a separator
            if Cursor mod Step = 0 then
               -- We should match a separator
               if C /= '_' then
                  return False;
               end if;
            else
               -- We should not match a separator
               if C = '_' then
                  return False;
               end if;
            end if;
            Cursor := Cursor + 1;
         end loop;
         return True;
      end Check_Separators;


      procedure Process_Number_Separator is
         use Framework.Locations, Framework.Reports;

         Name : constant Wide_String := Value_Image (Expression);

         Base_Delimiter_1 : Integer;
         Base_Delimiter_2 : Integer;
         Dot_Delimiter    : Integer;
         Exp_Delimiter    : Integer;

         -- Reverse a Wide_String
         function Wide_String_Reverse (S : in Wide_String) return Wide_String is
            Result : Wide_String (S'Range);
         begin
            for Idx in Result'Range loop
               Result (Idx) := S (S'Last - Idx + S'First);
            end loop;
            return Result;
         end Wide_String_Reverse;
      begin  -- Process_Number_Separator

         Number_Decomposition (Name, Base_Delimiter_1, Base_Delimiter_2, Dot_Delimiter, Exp_Delimiter);
         declare
            The_Base_Part     : constant Wide_String := Name (Name'First           .. Base_Delimiter_1 - 1);
            The_Integer_Part  : constant Wide_String := Name (Base_Delimiter_1 + 1 .. Dot_Delimiter    - 1);
            The_Decimal_Part  : constant Wide_String := Name (Dot_Delimiter    + 1 .. Base_Delimiter_2 - 1);

            The_Base          : constant Wide_String        := Utilities.Choose (Condition  => The_Base_Part = "",
                                                                                 When_True  => "10",
                                                                                 When_False => The_Base_Part);
            Context           : constant Root_Context'Class := Corresponding_Context (St_Numeric_Literal,
                                                                                      " " & The_Base);

            Block_Size        : Asis.ASIS_Positive;
         begin
            if Context = No_Matching_Context then
               -- Nothing specified for this base
               return;
            end if;

            -- Check for authorized base representations
            if Literal_Context (Context).Is_Not then
               Report (Rule_Id,
                       Context,
                       Get_Location (Expression),
                       "based representation not allowed");
            else
               -- Check that the number representation is as specified by the convention
               Block_Size := Literal_Context (Context).Block_Size;
               if        not Check_Separators (Wide_String_Reverse (The_Integer_Part), Block_Size)
                 or else not Check_Separators (The_Decimal_Part, Block_Size)
               then
                  Report (Rule_Id,
                          Context,
                          Get_Location (Expression),
                          "number representation does not follow convention");
               end if;
            end if;
         end;
      end Process_Number_Separator;

      procedure Process_Exponent is
         use Framework.Locations, Framework.Reports;

         Name : constant Wide_String := Value_Image (Expression);

         Base_Delimiter_1 : Character_Position;
         Base_Delimiter_2 : Character_Position;
         Dot_Delimiter    : Character_Position;
         Exp_Delimiter    : Character_Position;
      begin
         Number_Decomposition (Name, Base_Delimiter_1, Base_Delimiter_2, Dot_Delimiter, Exp_Delimiter);

         if Exp_Delimiter not in Name'Range then -- no exponent
            return;
         end if;

         if Name (Exp_Delimiter) = 'e' and Casing_Policy (St_Casing_Exponent) (Ca_Uppercase) then
            Report (Rule_Id,
                    Corresponding_Context (St_Casing_Exponent),
                    Get_Location (Expression) + Exp_Delimiter - 1,
                    "Wrong casing of exponent, should be 'E'");
            Fixes.Replace (From => Get_Location (Expression) + Exp_Delimiter - 1, Length => 1, By => "E");
         elsif Name (Exp_Delimiter) = 'E' and Casing_Policy (St_Casing_Exponent) (Ca_Lowercase) then
            Report (Rule_Id,
                    Corresponding_Context (St_Casing_Exponent),
                    Get_Location (Expression) + Exp_Delimiter - 1,
                    "Wrong casing of exponent, should be 'e'");
            Fixes.Replace (From => Get_Location (Expression) + Exp_Delimiter - 1, Length => 1, By => "e");
         end if;
      end Process_Exponent;

      procedure Process_Number_Casing is
         use Framework.Locations, Framework.Reports;

         Name : constant Wide_String := Value_Image (Expression);

         Base_Delimiter_1 : Character_Position;
         Base_Delimiter_2 : Character_Position;
         Dot_Delimiter    : Character_Position;
         Exp_Delimiter    : Character_Position;
      begin
         Number_Decomposition (Name, Base_Delimiter_1, Base_Delimiter_2, Dot_Delimiter, Exp_Delimiter);

         if Base_Delimiter_1 not in Name'Range then -- not a based number, certainly no letters!
            return;
         end if;

         for C : Wide_Character of Name (Positive (Base_Delimiter_1) + 1 .. Positive (Base_Delimiter_2) - 1) loop
            if C in 'a'..'f' and Casing_Policy (St_Casing_Number) (Ca_Uppercase) then
               Report (Rule_Id,
                       Corresponding_Context (St_Casing_Number),
                       Get_Location (Expression) + Base_Delimiter_1,
                       "Wrong casing of extended digit(s), should be "
                       & To_Upper (Name (Base_Delimiter_1 + 1 .. Base_Delimiter_2 - 1)));
               Fixes.Replace (From   => Get_Location (Expression) + Base_Delimiter_1,
                              Length => Base_Delimiter_2 - Base_Delimiter_1 - 1,
                              By     => To_Upper (Name (Base_Delimiter_1 + 1 .. Base_Delimiter_2 - 1)));
               return;
            elsif C in 'A' .. 'F' and Casing_Policy (St_Casing_Number) (Ca_Lowercase) then
               Report (Rule_Id,
                       Corresponding_Context (St_Casing_Exponent),
                       Get_Location (Expression) + Base_Delimiter_1,
                         "Wrong casing of extended digit(s), should be "
                         & To_Lower (Name (Base_Delimiter_1 + 1 .. Base_Delimiter_2 - 1)));
               Fixes.Replace (From   => Get_Location (Expression) + Base_Delimiter_1 + 1,
                              Length => Base_Delimiter_2 - Base_Delimiter_1 + 1,
                              By     => To_Lower (Name (Base_Delimiter_1 + 1 .. Base_Delimiter_2 - 1)));
               return;
            end if;
         end loop;
      end Process_Number_Casing;

      procedure Process_Exposed_Literal is
         use Framework.Locations, Framework.Reports, Literal_Flag_Utilities, Thick_Queries;
         use Asis, Asis.Elements;

         function Normalize (S : Wide_String) return Wide_String is
            -- Get rid of initial spaces and surrounding quotes, change double double-quotes to single ones
            use Ada.Strings.Wide_Fixed;

            Result      : Wide_String (S'Range);
            Inx_Out     : Natural := S'First-1;
            Ignore_Next : Boolean := False;
         begin
            for C : Wide_Character of S (Index (S, """")+1 .. S'Last-1) loop
               if Ignore_Next then
                  Ignore_Next := False;
               elsif C = '"' then
                  Ignore_Next := True;
               else
                  Inx_Out := Inx_Out + 1;
                  Result (Inx_Out) := C;
               end if;
            end loop;
            return Result (Result'First .. Inx_Out);
         end Normalize;

         function Get_Place return Place_Names is
         -- Search the place where the expression is used
            use Asis.Declarations;
            E        : Asis.Element := Expression;
            Top_Expr : Asis.Expression;
         begin
            loop
               case Element_Kind (E) is
                  when An_Expression =>
                     Top_Expr := E;
                     E := Enclosing_Element (E);
                  when An_Association | A_Definition =>
                     E := Enclosing_Element (E);
                  when A_Path =>
                     exit when Path_Kind (E) not in An_Expression_Path;
                     E := Enclosing_Element (E);
                  when others =>
                     exit;
               end case;
            end loop;

            case Element_Kind (E) is
               when A_Declaration =>
                  case Declaration_Kind (E) is
                     when A_Constant_Declaration =>
                        return Pl_Constant;
                     when A_Number_Declaration =>
                        return Pl_Number;
                     when A_Variable_Declaration =>
                        if Is_Equal (Top_Expr, Initialization_Expression (E)) then
                           return Pl_Var_Init;
                        else
                           return Pl_Declaration;
                        end if;
                     when A_Type_Declaration |
                          A_Subtype_Declaration
                          =>
                        return Pl_Type;
                     when A_Loop_Parameter_Specification =>
                        -- Although this one is formally a declaration, casual (non lawyer) people expect it
                        -- to be part of a (loop) statement
                        return Pl_Statement;
                     when others =>
                        return Pl_Declaration;
                  end case;
               when A_Clause =>
                  case Clause_Kind (E) is
                     when A_Representation_Clause | A_Component_Clause =>
                        return Pl_Repr_Clause;
                     when others =>
                        return Pl_Declaration;   -- For the sake of the casual user, consider a clause a declaration
                  end case;
               when A_Path =>
                  if Path_Kind (E) in A_Statement_Path then
                     return Pl_Statement;
                  end if;
                  -- Expression paths should have been caught above...
                  Failure ("Exposed_Literal: bad path", E);
               when A_Pragma =>
                  return Pl_Pragma;
               when A_Statement =>
                  return Pl_Statement;
               when others =>
                  return Pl_Other;
            end case;
         end Get_Place;

         Enclosing : Asis.Element;
         Negative  : Boolean;
         Place     : constant Place_Names := Get_Place;
      begin  -- Process_Exposed_Literal

         -- Note that if there is no check for the corresponding class of type,
         -- Report will be called with an Empty_Context (and thus will not report).
         case Expression_Kind (Expression) is
            when An_Integer_Literal =>
               if Permitted_Places (Lit_Integer) (Place) then
                  -- Always allowed
                  return;
               end if;
               if Permitted_Places (Lit_Integer) (Pl_Declaration)
                 and then Place in Other_Declarations
               then
                  return;
               end if;

               -- Find immediately enclosing expression, but get rid of spurious parentheses
               -- If the literal is part of a range, go up one more level (ranges cannot be parenthesized)
               Enclosing := Enclosing_Element (Expression);
               while Expression_Kind (Enclosing) = A_Parenthesized_Expression loop
                  Enclosing := Enclosing_Element (Enclosing);
               end loop;
               if Definition_Kind (Enclosing) in A_Discrete_Range then
                  Enclosing := Enclosing_Element (Enclosing);
               end if;

               if Permitted_Places (Lit_Integer) (Pl_Index)
                 and then Expression_Kind (Enclosing) = An_Indexed_Component
               then
                  return;
               end if;

               if Permitted_Places (Lit_Integer) (Pl_Aggr_Index)
                 and then Association_Kind (Enclosing) = An_Array_Component_Association
                 and then Is_Part_Of (Expression, Array_Component_Choices (Enclosing))
               then
                  return;
               end if;

               if Permitted_Places (Lit_Integer) (Pl_Attr_Index)
                 and then Expression_Kind (Enclosing) = An_Attribute_Reference
               then
                  return;
               end if;

               if Permitted_Places (Lit_Integer) (Pl_Exponent)
                 and then Association_Kind (Enclosing) = A_Parameter_Association
                 and then Operator_Kind (Called_Simple_Name (Enclosing_Element (Enclosing))) = An_Exponentiate_Operator
                 and then Is_Equal (Enclosing, Actual_Parameters (Enclosing_Element (Enclosing)) (2))
               then
                  return;
               end if;

               -- Compare to allowed values

               -- Check if negative: formally it's a unary minus applied to a positive literal, but the casual user
               -- understands it as a negative value.
               -- Is the Expression in an association in a function_call to a unary minus?
               Enclosing := Enclosing_Element (Expression);
               if Association_Kind (Enclosing) = A_Parameter_Association then
                  Enclosing := Enclosing_Element (Enclosing);
                  Negative := Expression_Kind (Enclosing) = A_Function_Call
                              and then Operator_Kind (Called_Simple_Name (Enclosing)) = A_Unary_Minus_Operator;
               else
                  Negative := False;
               end if;

               declare
                  type Truly_Biggest_Int is mod System.Max_Binary_Modulus;
                  -- We need this special type here instead of Biggest_Int, because
                  -- it cannot be non-static (it is a literal), and the user may well spell-out the full value
                  -- of System.Max_Binary_Modulus. However, if it is above Sytem.Max_Int, it is necessarily not
                  -- allowed, since Integer_Max_Value is bounded by System.Max_Int.
                  -- NB: if this code appears to be contrieved, consider checking the case of a modular value of
                  --     System.Max_Binary_Modulus without raising Constraint_Error...
                  I : Truly_Biggest_Int;
                  Value_Str : constant Wide_String := Value_Image (Expression);
               begin
                  I := Truly_Biggest_Int'Wide_Value (Value_Str);
                  if Integer_Max_Value /= Uninitialized
                    and then I <= Truly_Biggest_Int (Integer_Max_Value)
                  then
                     -- OK just return
                     return;
                  end if;

                  for K : Biggest_Int of Integer_Permitted_Values (1 .. Integer_Count) loop
                     if (K < 0) = Negative                      -- Both have same sign
                        and then Truly_Biggest_Int (abs K) = I  -- and same abs value
                     then
                        -- OK just return
                        return;
                     end if;
                  end loop;
               end;

               Report (Rule_Id,
                       Corresponding_Context (St_Exposed_Literal, Image (Lit_Integer, Lower_Case)),
                       Get_Location (Expression),
                       "integer literal "
                       & Choose (Negative, "-", "") & Trim_All (Element_Image (Expression))
                       & " not in allowed construct");

            when A_Real_Literal =>
               if Permitted_Places (Lit_Real) (Place) then
                  -- Always allowed
                  return;
               end if;
               if Permitted_Places (Lit_Real) (Pl_Declaration)
                 and then Place in Other_Declarations
               then
                  return;
               end if;


               -- Compare to allowed values with a delta possible
               -- due to rounding conversion problems

               -- Check if negative (see above)
               Enclosing := Enclosing_Element (Enclosing_Element (Expression));
               Negative := Expression_Kind (Enclosing) = A_Function_Call
                           and then Operator_Kind (Called_Simple_Name (Enclosing)) = A_Unary_Minus_Operator;
               declare
                  F : Float;
               begin
                  if Negative then
                     F := Float'Wide_Value ("-" & Value_Image (Expression));
                  else
                     F := Float'Wide_Value (Value_Image (Expression));
                  end if;
                  for K : Float of Real_Permitted_Values (1 .. Real_Count) loop
                     if abs(K - F) < 2.0*Float'Model_Epsilon then
                        -- OK just return
                        return;
                     end if;
                  end loop;
                  -- After running in the permitted values, nothing found
                  -- Put a report
                  Report (Rule_Id,
                          Corresponding_Context (St_Exposed_Literal, Image (Lit_Real, Lower_Case)),
                          Get_Location (Expression),
                          "real literal "
                          & Choose (Negative, "-", "") & Trim_All (Element_Image (Expression))
                          & " not in allowed construct");
               end;

            when A_Character_Literal =>
               if Permitted_Places (Lit_Character) (Place) then
                  -- Always allowed
                  return;
               end if;
               if Permitted_Places (Lit_Character) (Pl_Declaration)
                 and then Place in Other_Declarations
               then
                  return;
               end if;

               -- Find immediately enclosing expression, but get rid of spurious parentheses
               Enclosing := Enclosing_Element (Expression);
               while Expression_Kind (Enclosing) = A_Parenthesized_Expression loop
                     Enclosing := Enclosing_Element (Enclosing);
               end loop;
               if Permitted_Places (Lit_Character) (Pl_Index)
                 and then Expression_Kind (Enclosing) = An_Indexed_Component
               then
                  return;
               end if;
               Report (Rule_Id,
                       Corresponding_Context (St_Exposed_Literal, Image (Lit_Character, Lower_Case)),
                       Get_Location (Expression),
                       "character literal "
                       & Trim_All (Element_Image (Expression))
                       & " not in allowed construct");

            when A_String_Literal =>
               if Permitted_Places (Lit_String) (Place) then
                  -- Always allowed
                  return;
               end if;
               if Permitted_Places (Lit_String) (Pl_Declaration)
                 and then Place in Other_Declarations
               then
                  return;
               end if;

               -- Compare to allowed values
               declare
                  use String_Matching, Ada.Strings.Wide_Unbounded;
                  Good_Image : constant Wide_String := Normalize (Element_Image (Expression));
               begin
                  for S : Unbounded_Wide_String of String_Permitted_Values (1 .. String_Count) loop
                     if Match (Good_Image, To_Wide_String (S)) then
                        -- OK just return
                        return;
                     end if;
                  end loop;

                  Report (Rule_Id,
                          Corresponding_Context (St_Exposed_Literal, Image (Lit_String, Lower_Case)),
                          Get_Location (Expression),
                          "string literal "
                          & Trim_All (Element_Image (Expression))
                          & " not in allowed construct");
               end;

            when others =>
               Failure ("Unexpected literal");
         end case;

      end Process_Exposed_Literal;

      use Asis, Asis.Elements;
   begin  -- Process_Literal
      if (Rule_Used and Usage_Flags'(  St_Numeric_Literal | St_Exposed_Literal
                                     | St_Casing_Exponent | St_Casing_Number => True,
                                     others                                  => False)) = No_Subrule
      then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (St_Numeric_Literal)
        and then Expression_Kind (Expression) in An_Integer_Literal .. A_Real_Literal
      then
         Process_Number_Separator;
      end if;

      if Rule_Used (St_Casing_Exponent)
        and then Expression_Kind (Expression) in An_Integer_Literal .. A_Real_Literal
      then
         Process_Exponent;
      end if;

      if Rule_Used (St_Casing_Number)
        and then Expression_Kind (Expression) in An_Integer_Literal .. A_Real_Literal
      then
         Process_Number_Casing;
      end if;

      if Rule_Used (St_Exposed_Literal) then
         Process_Exposed_Literal;
      end if;
   end Process_Literal;

   ----------------------
   -- Process_Renaming --
   ----------------------

   procedure Process_Renaming  (Ren : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports;

      Target : Asis.Expression;
      Def    : Asis.Defining_Name;
   begin
      if not Rule_Used (St_Renamed_Entity) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Target := Renamed_Entity (Ren);
      while Expression_Kind (Target) in A_Type_Conversion | A_Qualified_Expression loop
         Target := Converted_Or_Qualified_Expression (Target);
      end loop;
      if Expression_Kind (Target) = A_Selected_Component then
         Target := Selector (Target);
      end if;

      case Expression_Kind (Target) is
         when An_Indexed_Component
            | A_Slice
            | An_Explicit_Dereference
            | A_Function_Call
              =>
            -- These are not considered
            return;
         when An_Identifier
            | An_Enumeration_Literal
            | A_Character_Literal
            | An_Operator_Symbol
              =>
            Def := Corresponding_Name_Definition (Target);
            if Is_Nil (Def) then
               -- Predefined operators, attributes...
               Uncheckable (Rule_Id, False_Negative, Get_Location (Target), "renaming of predefined operation");
            elsif Declaration_Kind (Enclosing_Element (Def))
               in A_Discriminant_Specification .. A_Component_Declaration
            then
               -- Record (or protected) field => not considered
               return;
            end if;

            -- Here we have a good one
            Renamed_Entities.Push ((Get_Location (Ren), Names (Ren)(1), Def));

         when An_Attribute_Reference =>
            Uncheckable (Rule_Id, False_Negative, Get_Location (Target), "renaming of attribute");
         when others =>
            Failure ("Unexpected element in renaming", Target);
      end case;
   end Process_Renaming;

   ---------------------
   -- Process_Element --
   ---------------------

   procedure Process_Element (Element : in Asis.Element) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Statements;
      use Framework.Locations, Framework.Reports, Scope_Manager, Thick_Queries;
      use Multiple_Flag_Utilities;

      procedure Check_Special_Use_Clause (Use_Clause : Asis.Clause) is
         -- Special processing for use clauses in context clauses.
         -- Accept it if the preceding clause is a with clause,
         -- and every name in this use clause is also given in the with clause
         use Asis.Clauses, Asis.Expressions;

         All_Clauses : constant Context_Clause_List
           := Context_Clause_Elements (Compilation_Unit => Enclosing_Compilation_Unit (Use_Clause),
                                       Include_Pragmas  => True) ;
         -- We include pragmas to prevent allowing a pragma between with and use
         Clause_Pos : List_Index;
      begin
         -- Find where we are, to get the preceding clause
         for I in All_Clauses'Range loop
            if Is_Equal (Use_Clause, All_Clauses (I)) then
               Clause_Pos := I;
               exit;
            end if;
         end loop;
         -- Clause_Pos cannot be All_Clauses'First, since a use clause cannot appear first.

         -- Is previous clause a with clause?
         if Clause_Kind (All_Clauses (Clause_Pos - 1)) /= A_With_Clause then
            Report (Rule_Id,
                    Corresponding_Context (St_Multiple_Elements, Image (Mu_Clause)),
                    Get_Location (Use_Clause),
                    "use clause does not start line and does not come after matching with clause");
            Fixes.Break (Get_Location (Use_Clause),
                         Indent_New => A4G_Bugs.Element_Span (All_Clauses (Clause_Pos - 1)).First_Column);
            return;
         end if;

         declare
            Use_Names  : constant Asis.Name_List := Clause_Names (Use_Clause);
            With_Names : constant Asis.Name_List := Clause_Names (All_Clauses (Clause_Pos - 1));
            Use_Def    : Asis.Defining_Name;
            With_Def   : Asis.Defining_Name;
            Found      : Boolean;
         begin
            -- Check that all Use_Names are part of With_Names
            -- This is a horrible N**2 algorithm, but since it can reasonably be expected
            -- that in most cases N=1 ...
            for U : Asis.Name of Use_Names loop
               if Expression_Kind (U) = A_Selected_Component then
                  Use_Def := Corresponding_Name_Definition (Selector (U));
               else
                  Use_Def := Corresponding_Name_Definition (U);
               end if;

               Found := False;
               for W : Asis.Name of With_Names loop
                  if Expression_Kind (W) = A_Selected_Component then
                     With_Def := Corresponding_Name_Definition (Selector (W));
                  else
                     With_Def := Corresponding_Name_Definition (W);
                  end if;

                  if Is_Equal (Use_Def, With_Def) then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Report (Rule_Id,
                          Corresponding_Context (St_Multiple_Elements, Image (Mu_Clause)),
                          Get_Location (U),
                          "use clause does not start line and "
                          & Extended_Name_Image (U)
                          & " is not part of the preceding with clause");
                  Fixes.Break (Get_Location (Use_Clause),
                               Indent_New => A4G_Bugs.Element_Span (All_Clauses (Clause_Pos - 1)).First_Column);
               end if;
            end loop;
         end;
      end Check_Special_Use_Clause;

      function Indentation (Loc : Location) return Text.Character_Position is
      -- number of spaces at  begininng of first line of Loc
      -- return 0 for empty lines or lines containing only spaces
         use Asis.Text;

         First_Line               : constant Line_Number      := Get_First_Line (Loc);
         Element_Lines            : constant Line_List (1..1) := Lines (Element, First_Line, First_Line);
         Element_First_Line_Image : constant Wide_String      := Line_Image (Element_Lines (Element_Lines'First));
      begin
         for I in Element_First_Line_Image'Range loop   --## Rule line off Simplifiable_expressions
                                                        --   Gela-ASIS compatibility
            if Element_First_Line_Image (I) > ' ' then
               return I-1;
            end if;
         end loop;
         return 0;
      end Indentation;

      function Has_Non_Spaces_Ahead (Loc : Location) return Boolean is
      begin
         return Indentation (Loc) < Get_First_Column (Loc) - 1;
      end Has_Non_Spaces_Ahead;

      function Actual_Stmt_Start_Loc (Stmt : Asis.Statement) return Location is
      -- Finds the "true" beginning of a statement, i.e. after labels and names
      -- Note that names are textually after labels!
      begin
         if Statement_Kind (Stmt) in A_Loop_Statement .. A_Block_Statement then
            -- those that can have names
            declare
               Name : constant Asis.Defining_Name := Statement_Identifier (Stmt);
            begin
               if not Is_Nil (Name) then
                  return Get_Next_Word_Location (Name, Starting => From_Tail);
               end if;
            end;
         end if;

         declare
            Labels : constant Asis.Defining_Name_List := Label_Names (Stmt);
         begin
            if Is_Nil (Labels) then
               return Get_Location (Stmt);
            end if;
            return Get_Next_Word_Location (Labels, Starting => From_Tail);
         end;
      end Actual_Stmt_Start_Loc;

      procedure Check_Split_End (End_Loc : Location) is
      -- Check that constructs that have an "end" have it on the same line as
      -- the end of Element (i.e. than "end XXX;" is not split on several lines
      begin
         if Get_First_Line (End_Loc) /= Get_First_Line (Get_End_Location (Element)) then
            case Element_Kind (Element) is
               when A_Declaration =>
                  Report
                    (Rule_Id,
                     Corresponding_Context (St_Multiple_Elements, Image (Mu_End, Lower_Case)),
                     End_Loc,
                     "closing keywords of declaration split");
               when A_Statement =>
                  Report
                    (Rule_Id,
                     Corresponding_Context (St_Multiple_Elements, Image (Mu_End, Lower_Case)),
                     End_Loc,
                     "closing keywords of statement split");
               when others =>
                  Failure (Rule_Id & ": no end for element", Element);
            end case;
         end if;
      end Check_Split_End;

      use Asis.Compilation_Units;
      Loc : Location;
   begin -- Process_Element
      if not Rule_Used (St_Multiple_Elements) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Check that the element starts a line

      -- Special case:
      -- if Element is the declaration of a private compilation unit, consider it starts
      -- at the preceding "private"
      if Is_Compilation_Unit (Element)
        and then Unit_Class (Enclosing_Compilation_Unit (Element)) = A_Private_Declaration
      then
         Loc := Get_Previous_Word_Location (Element, "PRIVATE");
      else
         Loc := Get_Location (Element);
      end if;

      if Has_Non_Spaces_Ahead (Loc) then
         case Element_Kind (Element) is
            when A_Clause =>
               if Flexible_Clause
                 and then Clause_Kind (Element) = A_Use_Package_Clause
                 and then In_Context_Clauses
               then
                  Check_Special_Use_Clause (Element);
               else
                  Report (Rule_Id,
                          Corresponding_Context (St_Multiple_Elements, Image (Mu_Clause)),
                          Loc,
                          "clause does not start line");
                  Fixes.Break (Place => Loc, Indent_New => Indentation (Loc));
               end if;

            when A_Declaration =>
               case Declaration_Kind (Element) is
                  when Not_A_Declaration =>
                     Failure (Rule_Id & ": Not_A_Declaration");
                  when An_Enumeration_Literal_Specification
                     | A_Discriminant_Specification
                     | A_Loop_Parameter_Specification
                     | A_Generalized_Iterator_Specification
                     | An_Element_Iterator_Specification
                     | A_Parameter_Specification
                     | An_Entry_Index_Specification
                     | A_Choice_Parameter_Specification
                     | A_Return_Constant_Specification
                     | A_Return_Variable_Specification
                     =>
                     -- These are allowed to appear on the same line as something else
                     null;
                  when others =>
                     Report
                       (Rule_Id,
                        Corresponding_Context (St_Multiple_Elements, Image (Mu_Declaration, Lower_Case)),
                        Loc,
                        "declaration does not start line");
                     Fixes.Break (Place => Loc, Indent_New => Indentation (Loc));
               end case;

            when A_Statement =>
               Report (Rule_Id,
                       Corresponding_Context (St_Multiple_Elements, Image (Mu_Statement, Lower_Case)),
                       Loc,
                       "statement does not start line");
               Fixes.Break (Place => Loc, Indent_New => Indentation (Loc));

            when A_Pragma =>
               Report (Rule_Id,
                       Corresponding_Context (St_Multiple_Elements, Image (Mu_Pragma, Lower_Case)),
                       Loc,
                       "pragma does not start line");
               Fixes.Break (Place => Loc, Indent_New => Indentation (Loc));

            when others =>
               Failure (Rule_Id & ": inappropriate element kind");
         end case;
      end if;

      -- Check keywords
      case Declaration_Kind (Element) is
         when A_Procedure_Body_Declaration
            | A_Function_Body_Declaration
            | A_Package_Body_Declaration
            | A_Task_Body_Declaration
            | An_Entry_Body_Declaration
            =>
            Loc := Get_Next_Word_Location (Element, Matching => "IS", Starting => From_Head);
            if Get_First_Line (Loc) /=  Get_First_Line (Get_Location (Element))
              and then Has_Non_Spaces_Ahead (Loc)
            then
               Report (Rule_Id,
                       Corresponding_Context (St_Multiple_Elements, Image (Mu_Is, Lower_Case)),
                       Loc,
                       """is"" does not start line");
               Fixes.Break (Loc, Indentation (Get_Location (Element)));
            end if;

            declare
               Stmts : constant Asis.Statement_List := Body_Statements (Element, Include_Pragmas => True);
            begin
               if Stmts /= Nil_Element_List then
                  Loc := Get_Previous_Word_Location (Stmts, Starting => From_Head);
                  if Has_Non_Spaces_Ahead (Loc) then
                     Report (Rule_Id,
                             Corresponding_Context (St_Multiple_Elements, Image (Mu_Begin, Lower_Case)),
                             Loc,
                             """begin"" does not start line");
                     Fixes.Break (Loc, Indentation (Get_Location (Element)));
                  end if;
                  Loc := Get_Next_Word_Location (Stmts, Starting => From_Tail);

                  declare
                     Handlers : constant Asis.Exception_Handler_List := Body_Exception_Handlers
                                                                          (Element, Include_Pragmas => True);
                  begin
                     if Handlers /= Nil_Element_List then
                        Loc := Get_Previous_Word_Location (Handlers, Starting => From_Head);
                        if Has_Non_Spaces_Ahead (Loc) then
                           Report (Rule_Id,
                                   Corresponding_Context (St_Multiple_Elements, Image (Mu_Handler, Lower_Case)),
                                   Loc,
                                   """exception"" does not start line");
                           Fixes.Break (Loc, Indentation (Get_Location (Element)));
                        end if;

                        for H : Asis.Exception_Handler of Handlers loop
                           Loc := Get_Location (H);
                           if Has_Non_Spaces_Ahead (Loc) then
                              Report (Rule_Id,
                                      Corresponding_Context (St_Multiple_Elements, Image (Mu_Handler, Lower_Case)),
                                      Loc,
                                      """when"" does not start line");
                              Fixes.Break (Loc, Indentation (Loc));     -- Not obvious where to pick indentation from
                           end if;
                        end loop;

                        Loc := Get_Next_Word_Location (Handlers, Starting => From_Tail);
                     end if;
                  end;

                  -- Here, Loc is the position of "end", either as word that follows the statements,
                  -- or if there are exception handlers, as the word that follows the last exception
                  -- handler
                  if Has_Non_Spaces_Ahead (Loc) then
                     Report (Rule_Id,
                             Corresponding_Context (St_Multiple_Elements, Image (Mu_End, Lower_Case)),
                             Loc,
                             """end"" does not start line");
                     Fixes.Break (Loc, Indentation (Get_Location (Element)));
                  end if;
                  Check_Split_End (Loc);
               end if;
            end;
         when others =>
            null;
      end case;

      -- For statements, check that "then", "is", "loop", "do" are on the same line as Element, or start a line
      case Statement_Kind (Element) is
         when An_If_Statement =>
            declare
               Paths : constant Path_List := Statement_Paths (Element, Include_Pragmas => True);
            begin
               Loc := Get_Previous_Word_Location (Thick_Queries.Statements (Paths (1), Include_Pragmas => True),
                                                  Starting => From_Head);
               if Get_First_Line (Loc) /=  Get_First_Line (Actual_Stmt_Start_Loc (Element))
                 and then Has_Non_Spaces_Ahead (Loc)
               then
                  Report (Rule_Id,
                          Corresponding_Context (St_Multiple_Elements, Image (Mu_Then, Lower_Case)),
                          Loc,
                          """then"" does not start line");
                  Fixes.Break (Loc, Indentation (Get_Location (Element)));
               end if;

               for P : Asis.Path of Paths (2 .. Paths'Last) loop
                  Loc := Get_Location (P);
                  if Has_Non_Spaces_Ahead (Loc) then
                     Report (Rule_Id,
                             Corresponding_Context (St_Multiple_Elements, Image (Mu_Else, Lower_Case)),
                             Loc,
                             """elsif/else"" does not start line");
                     Fixes.Break (Loc, Indentation (Get_Location (Element)));
                  end if;
                  Loc := Get_Previous_Word_Location (Thick_Queries.Statements (P, Include_Pragmas => True),
                                                     Starting => From_Head);
                  if Get_First_Line (Loc) /=  Get_First_Line (Get_Location (P))
                    and then Has_Non_Spaces_Ahead (Loc)
                  then
                     Report (Rule_Id,
                             Corresponding_Context (St_Multiple_Elements, Image (Mu_Then, Lower_Case)),
                             Loc,
                             """then"" does not start line");
                     Fixes.Break (Loc, Indentation (Get_Location (Element)));
                  end if;
               end loop;

            end;

         when A_Case_Statement =>
            declare
               Paths : constant Path_List := Statement_Paths (Element, Include_Pragmas => True);
            begin
               Loc := Get_Previous_Word_Location (Paths, Starting => From_Head);
               if Get_First_Line (Loc) /= Get_First_Line (Actual_Stmt_Start_Loc (Element))
                 and then Has_Non_Spaces_Ahead (Loc)
               then
                  Report (Rule_Id,
                          Corresponding_Context (St_Multiple_Elements, Image (Mu_Is, Lower_Case)),
                          Loc,
                          """is"" does not start line");
                  Fixes.Break (Loc, Indentation (Get_Location (Element)));
               end if;

               for P : Asis.Path of Paths loop
                  Loc := Get_Location (P);
                  if Has_Non_Spaces_Ahead (Loc) then
                     Report (Rule_Id,
                             Corresponding_Context (St_Multiple_Elements, Image (Mu_When, Lower_Case)),
                             Loc,
                             """when"" does not start line");
                     Fixes.Break (Loc, Indentation (Get_Location (Element)));
                  end if;
               end loop;
            end;

         when A_Loop_Statement
            | A_While_Loop_Statement
            | A_For_Loop_Statement
            =>
            declare
               Stmts : constant Asis.Statement_List := Thick_Queries.Statements (Element, Include_Pragmas => True);
            begin
               Loc := Get_Previous_Word_Location (Stmts, Starting => From_Head);
               if Get_First_Line (Loc) /= Get_First_Line (Actual_Stmt_Start_Loc (Element))
                 and then Has_Non_Spaces_Ahead (Loc)
               then
                  Report (Rule_Id,
                          Corresponding_Context (St_Multiple_Elements, Image (Mu_Loop, Lower_Case)),
                          Loc,
                          """loop"" does not start line");
                  Fixes.Break (Loc, Indentation (Get_Location (Element)));
               end if;
            end;

         when An_Accept_Statement
            | An_Extended_Return_Statement
            =>
            declare
               Stmts : constant Asis.Statement_List := Thick_Queries.Statements (Element, Include_Pragmas => True);
            begin
               if Stmts /= Nil_Element_List then  -- Statements are optional for accept and extended return
                  Loc := Get_Previous_Word_Location (Stmts, Starting => From_Head);
                  if Get_First_Line (Loc) /= Get_First_Line (Get_Location (Element))
                    and then Has_Non_Spaces_Ahead (Loc)
                  then
                     Report (Rule_Id,
                             Corresponding_Context (St_Multiple_Elements, Image (Mu_Do, Lower_Case)),
                             Loc,
                             """do"" does not start line");
                     Fixes.Break (Loc, Indentation (Get_Location (Element)));
                  end if;
               end if;
            end;

         when A_Block_Statement =>
            if Is_Declare_Block (Element) then -- otherwise the message is already issued
               declare
                  Stmts : constant Asis.Statement_List := Block_Statements (Element, Include_Pragmas => True);
               begin
                  Loc := Get_Previous_Word_Location (Stmts, Starting => From_Head);
                  if Has_Non_Spaces_Ahead (Loc) then
                     Report (Rule_Id,
                             Corresponding_Context (St_Multiple_Elements, Image (Mu_Begin, Lower_Case)),
                             Loc,
                          """begin"" does not start line");
                     Fixes.Break (Loc, Indentation (Get_Location (Element)));
                  end if;
               end;
            end if;

         when others =>
            null;
      end case;

      -- For statements, Check that corresponding "end" starts a line
      case Statement_Kind (Element) is
         when An_If_Statement
            | A_Case_Statement
            | A_Loop_Statement
            | A_While_Loop_Statement
            | A_For_Loop_Statement
            | A_Block_Statement
            | A_Selective_Accept_Statement
            | A_Timed_Entry_Call_Statement
            | A_Conditional_Entry_Call_Statement
            | An_Asynchronous_Select_Statement
            =>
            Loc := Get_Previous_Word_Location (Element, Matching => "END", Starting => From_Tail);
            if Has_Non_Spaces_Ahead (Loc) then
               Report (Rule_Id,
                       Corresponding_Context (St_Multiple_Elements, Image (Mu_End, Lower_Case)),
                       Loc,
                       """end"" does not start line");
               Fixes.Break (Loc, Indentation (Get_Location (Element)));
            end if;
            Check_Split_End (Loc);

         when An_Accept_Statement  -- Statements are optional...
            | An_Extended_Return_Statement
            =>
            if Thick_Queries.Statements (Element, Include_Pragmas => True) /= Nil_Element_List then
               Loc := Get_Previous_Word_Location (Element, Matching => "END", Starting => From_Tail);
               if Has_Non_Spaces_Ahead (Loc) then
                  Report (Rule_Id,
                          Corresponding_Context (St_Multiple_Elements, Image (Mu_End, Lower_Case)),
                          Loc,
                          """end"" does not start line");
                  Fixes.Break (Loc, Indentation (Get_Location (Element)));
               end if;
               Check_Split_End (Loc);
            end if;

         when others =>
            null;
      end case;
   end Process_Element;

   --------------------
   -- Process_Aspect --
   --------------------

   procedure Process_Aspect (Aspect : Asis.Definition) is
      use Asis.Definitions;
   begin
      if not Rule_Used (St_Casing_Aspect) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Casing (St_Casing_Aspect, Aspect_Mark (Aspect));
   end Process_Aspect;

   -----------------------
   -- Process_Attribute --
   -----------------------

   procedure Process_Attribute (Attribute : in Asis.Expression) is
      use Asis.Expressions;
      Identifier : Asis.Expression;
  begin
      if not Rule_Used (St_Casing_Attribute) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Identifier := Attribute_Designator_Identifier (Attribute);
      Check_Casing (St_Casing_Attribute, Identifier);
   end Process_Attribute;

   --------------------
   -- Process_Pragma --
   --------------------

   procedure Process_Pragma (Pr : in Asis.Pragma_Element) is
   begin
      if not Rule_Used (St_Casing_Pragma) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Casing (St_Casing_Pragma, Pr);
   end Process_Pragma;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Locations.Location) is
   begin
      if not Rule_Used (St_Casing_Keyword) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Rules.Style.Keyword.Process_Line (Line, Loc, Casing_Policy (St_Casing_Keyword));
   end Process_Line;

begin  -- Rules.Style
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic_Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Style;
