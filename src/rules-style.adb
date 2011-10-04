----------------------------------------------------------------------
--  Rules.Style - Package body                                      --
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
   Ada.Strings.Wide_Fixed,
   Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Clauses,
  Asis.Elements,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Expressions,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  String_Matching,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Scope_Manager,
  Rules.Style.Keyword;
pragma Elaborate (Framework.Language);

package body Rules.Style is

   use Framework, Framework.Control_Manager, Utilities;

   -- See declaration of Style_Names in the private part of the specification
   subtype Casing_Styles is Subrules range St_Casing_Attribute .. St_Casing_Pragma;
   type Usage_Flags is array (Subrules) of Boolean;

   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Subrules,
                                                                             Prefix => "St_" );

   --
   -- Parameters for the casing subrule
   --

   -- See declaration of Casing_Names in the private part of the specification
   package Casing_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Casing_Names,
                                                                           Prefix => "Ca_" );
   Casing_Policy : array (Casing_Styles) of Casing_Names;
   -- We use a simple variable here rather than going through the context,
   -- because the rule can be given only once, and efficiency is a concern
   -- (the rule is called on every identifier).


   --
   -- Declarations for the compound_statement subrule
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


   --
   -- Declarations for the exposed_literal subrule
   --

   type Literal_Names is (Lit_Integer, Lit_Real, Lit_Character, Lit_String);
   package Literal_Flag_Utilities  is new Framework.Language.Flag_Utilities (Flags => Literal_Names,
                                                                             Prefix => "Lit_" );
   type Place_Names is (Pl_Other,
                        Pl_Constant, Pl_Number,      Pl_Var_Init, Pl_Type,
                        Pl_Pragma,   Pl_Repr_Clause, Pl_Index,    Pl_Exponent);
   -- Pl_Other used internally when not in one of the other Places, not accessible to user. Must stay first.
   type Place_Set is array (Place_Names) of Boolean;
   No_Place : constant Place_Set := (others => False);
   package Place_Flag_Utilities  is new Framework.Language.Flag_Utilities (Flags => Place_Names,
                                                                           Prefix => "Pl_" );

   Nbr_Of_Permitted_Consts : constant := 20;
   type Permitted_Consts_Count is range 0 .. Nbr_Of_Permitted_Consts;
   subtype Permitted_Consts_Range is Permitted_Consts_Count range 1 .. Permitted_Consts_Count'Last;

   Integer_Permitted_Values : array (Permitted_Consts_Range) of Thick_Queries.Biggest_Int;
   Real_Permitted_Values    : array (Permitted_Consts_Range) of Float;
   String_Permitted_Values  : array (Permitted_Consts_Range) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Integer_Count            : Permitted_Consts_Count := 0;     -- Number of Integer parameters
   Real_Count               : Permitted_Consts_Count := 0;     -- Number of Real parameters
   String_Count             : Permitted_Consts_Count := 0;     -- Number of String parameters
   Permitted_Places         : array (Literal_Names) of Place_Set := (others => (others => False));


   --
   -- Declarations for the literal subrule
   --

   subtype Allowed_Bases is Positive range 2 .. 16;
   type Literal_Context is new Basic_Rule_Context with
      record
         Is_Not     : Boolean;
         Block_Size : Natural;
      end record;


   --
   -- Declarations for the multiple_elements subrule
   --

   type Multiple_Names is (Mu_Clause, Mu_Declaration, Mu_Statement);
   package Multiple_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags  => Multiple_Names,
                                                                             Prefix => "Mu_" );
   Flexible_Clause : Boolean;
   -- True if at least one Flexible


   --
   -- Declarations for the no_closing_name subrule
   --

   type Closing_Name_Context is new Basic_Rule_Context with
      record
         Length : Asis.ASIS_Integer;
      end record;


   --
   -- Declarations for the [formal_]parameter_order subrule
   --

   type Extended_Modes is (Mode_In,   Mode_Defaulted_In, Mode_Access,   Mode_In_Out, Mode_Out,
                           Mode_Type, Mode_Procedure,    Mode_Function, Mode_Package);
   package Extended_Modes_Utilities is new Framework.Language.Modifier_Utilities (Modifiers => Extended_Modes,
                                                                                  Prefix    => "Mode_");
   type Order_Index is range 1 .. Extended_Modes'Pos (Extended_Modes'Last) + 1 + 1;
   -- Number of orders that can be specified.
   -- Quite arbitrary; here we allow one position for each Extended_Mode, plus the extra
   -- guard value.
   type Modes_Array is array (Order_Index) of Extended_Modes_Utilities.Modifier_Set;
   Mode_Order : array (St_Orders) of Modes_Array;
   Order_Inx  : array (St_Orders) of Order_Index;


   --
   -- Declarations for the positional_association subrule
   --

   type Extended_Association_Names is (Na_No_Association,
                                       Na_Pragma,       Na_Call,            Na_Instantiation,
                                       Na_Discriminant, Na_Array_Aggregate, Na_Record_Aggregate);
   subtype Association_Names is Extended_Association_Names
           range Extended_Association_Names'Succ (Na_No_Association) .. Extended_Association_Names'Last;
   subtype Exceptionable_Association_Names is Association_Names range Na_Pragma .. Na_Instantiation;

   package Named_Parameter_Flag_Utilities is new Framework.Language.Flag_Utilities
     (Flags  => Extended_Association_Names,
      Prefix => "Na_" );

   type Association_Usage is array (Association_Names) of Boolean;
   Association_Used : Association_Usage := (others => False);

   type Association_Context is new Basic_Rule_Context with
      record
         Allowed_Number  : Natural;
         Except_Operator : Boolean;
      end record;

   Positional_Exceptions : array (Exceptionable_Association_Names) of Context_Store;
   -- A Context_Store of Null_Context to flag entities that need not obey the rule

   --
   -- Declarations for the renamed_entity subrule
   --

   type Renaming_Data is
      record
         Ren_Location : Location;
         Renamed_Def  : Asis.Defining_Name;
      end record;
   function Is_Same_Def (L, R : Renaming_Data) return Boolean;
   procedure Clear (Item : in out Renaming_Data) is  -- null proc
      pragma Unreferenced (Item);
   begin
      null;
   end Clear;
   package Renamed_Entities is new Framework.Scope_Manager.Scoped_Store (Renaming_Data,
                                                                         Equivalent_Keys => Is_Same_Def);

   --
   -- General parameters
   --

   Rule_Used : Usage_Flags := (others => False);
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

      User_Message ("For casing_*:");
      Casing_Flag_Utilities.Help_On_Flags (Header => "   Parameter (2):",
                                           Footer => "(default = Original)");

      User_Message ("For exposed_literal:");
      Literal_Flag_Utilities.Help_On_Flags (Header => "   Parameter (2)  :");
      Place_Flag_Utilities.Help_On_Flags (Header     => "   Parameter (3..):",
                                          Footer     => "(optional)",
                                          Extra_Value => "<value>");
      User_Message ("For multiple_elements:");
      Multiple_Flag_Utilities.Help_On_Flags (Header => "   Parameter (2..): [flexible]",
                                             Footer => "(default = all)");

      User_Message ("For no_closing_name:");
      User_Message ("   Parameter (2): maximum number of lines allowed");

      User_Message ("For numeric_literal:");
      User_Message ("   Parameter (2): [not] <base>");
      User_Message ("   Parameter (3): <block_size>");

      User_Message ("For parameter_order:");
      Extended_Modes_Utilities.Help_On_Modifiers(Header => "   parameter (2..): list of");

      User_Message ("For positional_association:");
      Named_Parameter_Flag_Utilities.Help_On_Flags
        (Header      => "   Parameter (2..): [not_operator]",
         Extra_Value => "",
         Footer      => "(default = all)");
      User_Message ("   each value may be followed by allowed number of occurrences");
      User_Message ("   and entities not required to follow the rule");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control(Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Ada.Strings.Wide_Unbounded;
      use Casing_Flag_Utilities, Literal_Flag_Utilities, Multiple_Flag_Utilities, Named_Parameter_Flag_Utilities;
      use Place_Flag_Utilities, Subrules_Flag_Utilities;

      Subrule   : Subrules;
      Max       : Integer;
      Except_Op : Boolean;
      Assoc     : Extended_Association_Names;
      Next_Assoc     : Extended_Association_Names;
      Multiple  : Multiple_Names;
      Lit_Kind  : Literal_Names;
      Places    : Place_Set := (others => False);
      P         : Place_Names;
      Flexible  :  Boolean;
   begin
      if Parameter_Exists then
         Subrule := Get_Flag_Parameter (Allow_Any => False);
         case Subrule is
            when St_Compound_Statement
              | St_Default_In
              | St_Negative_Condition
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
                  Casing_Policy (Subrule) := Get_Flag_Parameter (Allow_Any => False);
                  if Casing_Policy (Subrule) = Ca_Original and Subrule /= St_Casing_Identifier then
                     Parameter_Error (Rule_Id, """Original"" allowed only for identifiers");
                  end if;
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

                     case Lit_Kind is
                        when Lit_Integer =>
                           if Integer_Count = Permitted_Consts_Count'Last then
                              Parameter_Error (Rule_Id, "too many integer values");
                           end if;

                           Integer_Count := Integer_Count + 1;
                           Integer_Permitted_Values (Integer_Count) := Get_Integer_Parameter;
                        when Lit_Real =>
                           if Real_Count = Permitted_Consts_Count'Last then
                              Parameter_Error (Rule_Id, "too many real values");
                           end if;

                           Real_Count := Real_Count + 1;
                           Real_Permitted_Values (Real_Count) := Get_Float_Parameter;
                        when Lit_String =>
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

               if Places = (Place_Names => False) then
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

                     Associate (Contexts,
                                Value (Image (St_Multiple_Elements, Lower_Case) & Image (Multiple)),
                                Basic.New_Context (Ctl_Kind, Ctl_Label));
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
                  Block_Size : Natural;
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
                  Not_Specified : Modifier_Set := Full_Set;
               begin
                  if Rule_Used (Subrule) then
                     Parameter_Error (Rule_Id, Image (Subrule, Lower_Case) & " already specified for rule");
                  end if;
                  if Parameter_Exists then
                     Order_Inx (Subrule):= 1;
                     loop
                        Mode_Order (Subrule)(Order_Inx (Subrule)) := Get_Modifier_Set (No_Parameter => True);
                        Not_Specified := Not_Specified and not Mode_Order (Subrule)(Order_Inx (Subrule));
                        exit when not Parameter_Exists;
                        if Order_Inx (Subrule) = Order_Index'Last - 1 then
                           Parameter_Error (Rule_Id, "Too many parameters");
                        end if;
                        Order_Inx (Subrule) := Order_Inx (Subrule) + 1;
                     end loop;
                     if Not_Specified /= Empty_Set then
                        -- allow all modes not explicitely specified after the ones specified
                        Order_Inx  (Subrule):= Order_Inx (Subrule) + 1;
                        Mode_Order (Subrule)(Order_Inx (Subrule)) := Not_Specified;
                     end if;
                  else
                     case St_Orders (Subrule) is
                        when St_Parameter_Order =>
                           Mode_Order (St_Parameter_Order)
                             := ((Mode_In | Mode_Access => True, others => False),
                                 (Mode_In_Out           => True, others => False),
                                 (Mode_Out              => True, others => False),
                                 (Mode_Defaulted_In     => True, others => False),
                                 others =>                      (others => False));
                           Order_Inx (St_Parameter_Order) := 4;
                        when St_Formal_Parameter_Order =>
                           Mode_Order (St_Formal_Parameter_Order )
                             := ((Mode_Type                                 => True, others => False),
                                 (Mode_In | Mode_Access | Mode_Defaulted_In => True, others => False),
                                 (Mode_In_Out                               => True, others => False),
                                 (Mode_Procedure | Mode_Function            => True, others => False),
                                 (Mode_Package                              => True, others => False),
                                 others =>                                          (others => False));
                           Order_Inx (St_Formal_Parameter_Order) := 5;
                     end case;
                  end if;
               end;
               Rule_Used (Subrule) := True;
               Associate (Contexts, Value (Image (Subrule, Lower_Case)), Basic.New_Context (Ctl_Kind, Ctl_Label));

            when St_Positional_Association =>
               if Parameter_Exists and then not Is_Integer_Parameter then
                  Except_Op := Get_Modifier ("NOT_OPERATOR");
                  Assoc     := Get_Flag_Parameter (Allow_Any => False);

                  Association_Parameters :
                  loop
                     if Assoc = Na_No_Association then
                        -- This is possible if the user specified "No_Association" the first time,
                        -- or "not_operator no_association" later. Quite unlikely, but this is not
                        -- a reason to ignore that case
                        Parameter_Error (Rule_Id, "Not a valid parameter: No_Association");
                     end if;

                     if Except_Op and Assoc /= Na_Call then
                        Parameter_Error (Rule_Id, "Not_Operator can be specified only with ""call""");
                     end if;
                     Association_Used (Assoc) := True;

                     if Parameter_Exists and then Is_Integer_Parameter then
                        Max := Get_Integer_Parameter (Min => 0);
                     else
                        Max := 0;
                     end if;
                     Associate (Contexts,
                                Value (Image (St_Positional_Association) & Image (Assoc)),
                                Association_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Max, Except_Op));
                     exit when not Parameter_Exists;

                     Except_Op  := Get_Modifier ("NOT_OPERATOR");
                     Next_Assoc := Get_Flag_Parameter (Allow_Any => not Except_Op);

                     if not Except_Op and Next_Assoc = Na_No_Association then
                        -- exception entities
                        if Assoc not in Exceptionable_Association_Names then
                           Parameter_Error (Rule_Id, "no exempted entities allowed for """
                                            & Image (Assoc, Lower_Case) & '"');
                        end if;

                        loop
                           Associate (Positional_Exceptions (Assoc), Get_Entity_Parameter, Null_Context);
                           exit Association_Parameters when not Parameter_Exists;

                           Except_Op  := Get_Modifier ("NOT_OPERATOR");
                           Next_Assoc := Get_Flag_Parameter (Allow_Any => not Except_Op);
                           exit when Except_Op or Next_Assoc /= Na_No_Association;
                        end loop;
                     end if;
                     Assoc := Next_Assoc;
                  end loop Association_Parameters;
               else
                  if Parameter_Exists then
                     -- Must be integer parameter
                     Max := Get_Integer_Parameter (Min => 0);
                  else
                     Max := 0;
                  end if;
                  Association_Used := (others => True);
                  for A in Association_Names loop
                     Associate (Contexts,
                                Value (Image (St_Positional_Association) & Image (A)),
                                Association_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with Max, False));
                  end loop;
               end if;
               Rule_Used (St_Positional_Association) := True;
         end case;

      else
         -- No parameter => all style checks
         Rule_Used := (others => True);

         -- Casing_Attribute
         Associate (Contexts, Value (Image (St_Casing_Attribute)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Attribute) := Ca_Titlecase;

         -- Casing_Keyword
         Associate (Contexts, Value (Image (St_Casing_Keyword)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Keyword) := Ca_Lowercase;

         -- Casing_Identifier
         Associate (Contexts, Value (Image (St_Casing_Identifier)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Identifier) := Ca_Original;

         -- Casing_Pragma
         Associate (Contexts, Value (Image (St_Casing_Pragma)), Basic.New_Context (Ctl_Kind, Ctl_Label));
         Casing_Policy (St_Casing_Identifier) := Ca_Titlecase;

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

         -- Negative_Condition
         Associate (Contexts, Value (Image (St_Negative_Condition)), Basic.New_Context (Ctl_Kind, Ctl_Label));

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
         Mode_Order :=
           (St_Parameter_Order => ((Mode_In | Mode_Access => True, others => False),
                                   (Mode_In_Out           => True, others => False),
                                   (Mode_Out              => True, others => False),
                                   (Mode_Defaulted_In     => True, others => False),
                                others => (others => False)),
            St_Formal_Parameter_Order => ((Mode_Type                                 => True, others => False),
                                          (Mode_In | Mode_Access | Mode_Defaulted_In => True, others => False),
                                          (Mode_In_Out                               => True, others => False),
                                          (Mode_Procedure | Mode_Function            => True, others => False),
                                          (Mode_Package                              => True, others => False),
                                          others => (others => False)));
            Order_Inx := (St_Parameter_Order => 4, St_Formal_Parameter_Order => 5);

         -- Positional_Association
         Association_Used := (others => True);
         for A in Association_Names loop
            Associate (Contexts,
                       Value (Image (St_Positional_Association) & Image (A)),
                       Association_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label) with 0, False));
         end loop;

       end if;
   exception
      when Already_In_Store =>
         Parameter_Error (Rule_Id, "Subrule already provided: " & Image (Subrule, Lower_Case));
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Clear (Contexts);
            Rule_Used        := (others => False);
            Real_Count       := 0;
            Integer_Count    := 0;
            String_Count     := 0;
            Permitted_Places := (others => (others => False));
            Flexible_Clause  := False;
            for Assoc in Positional_Exceptions'Range loop
               Clear (Positional_Exceptions (Assoc));
            end loop;
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

   ------------------
   -- Check_Casing --
   ------------------

   procedure Check_Casing (Source_Name : in Wide_String;
                           Casing      : in Casing_Styles;
                           Element     : in Asis.Expression)
   is
      -- Element is the identifier for St_Casing_Identifier and St_Casing_Attribute
      -- and the pragma for St_Casing_Pragma
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Reports;

      Reference_Name : Wide_String (Source_Name'Range);
      -- Note that the source name and the refence name always have the same length!
      Def_Name : Asis.Defining_Name;
   begin
      case Casing_Policy (Casing) is
         when Ca_Uppercase =>
            Reference_Name := To_Upper (Source_Name);
         when Ca_Lowercase =>
            Reference_Name := To_Lower (Source_Name);
         when Ca_Titlecase =>
            Reference_Name := To_Title (Source_Name);
         when Ca_Original =>
            if Element_Kind (Element) = A_Defining_Name then
               -- Since it *is* the original...
               return;
            end if;

            Def_Name := Corresponding_Name_Definition (Element);
            if Is_Nil (Def_Name) then
               -- some predefined stuff, give up
               return;
            end if;

            if Defining_Name_Kind (Def_Name) = A_Defining_Expanded_Name then
               Def_Name := Defining_Selector (Def_Name);
            end if;

            Reference_Name := Defining_Name_Image (Def_Name);
      end case;

      if Source_Name /= Reference_Name then
         Report (Rule_Id,
                 Corresponding_Context (Casing),
                 Get_Location (Element),
                 "Wrong casing of """ & Source_Name & """, should be """ & Reference_Name & '"');
      end if;
   end Check_Casing;

   -----------------------
   -- Process_Construct --
   -----------------------

   -- Controls declarations that allow repeating the name at the end of the declaration, and where the closing name
   -- is omitted

   procedure Process_Construct (Construct : in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Text;
      use Framework.Reports;
      Length : Line_Number;
   begin
      if not Rule_Used (St_No_Closing_Name) then
         return ;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Construct) is
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
         Length := Last_Line_Number (Construct) - First_Line_Number (Construct) + 1;
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
      end if;

   end Process_Construct;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Identifier : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Declarations;
    procedure Check_Renamed is
         use Framework.Scope_Manager, Framework.Reports;

         Def : constant Asis.Definition := Corresponding_Name_Definition (Identifier);
         Ren : Renaming_Data;
      begin
         if Is_Nil (Def) then
            -- Predefined stuff
            return;
         end if;

         Renamed_Entities.Reset ((Null_Location, Def), All_Scopes);
         if not Renamed_Entities.Data_Available then
            return;
         end if;

         Ren := Renamed_Entities.Current_Data;
         Report (Rule_Id,
                 Corresponding_Context (St_Renamed_Entity),
                 Get_Location (Identifier),
                 Defining_Name_Image (Def) & " has been renamed at " & Image (Ren.Ren_Location));
      end Check_Renamed;

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
         if Rule_Used (St_Casing_Identifier) and then Defining_Name_Kind (Identifier) = A_Defining_Identifier then
            Check_Casing (Defining_Name_Image (Identifier), St_Casing_Identifier, Identifier);
         elsif Rule_Used (St_Casing_Keyword) and then Defining_Name_Kind (Identifier) = A_Defining_Operator_Symbol then
            Check_Casing (Defining_Name_Image (Identifier), St_Casing_Keyword, Identifier);
         end if;
      else
         if Rule_Used (St_Casing_Identifier) and then Expression_Kind (Identifier) = An_Identifier then
            Check_Casing (A4G_Bugs.Name_Image (Identifier), St_Casing_Identifier, Identifier);
         elsif Rule_Used (St_Casing_Keyword) and then Expression_Kind (Identifier) = An_Operator_Symbol then
            -- This is an operator, must be the prefix of a function call
            -- If it uses infix notation, don't handle it because it will be found by the texual rule for keywords
            if Is_Prefix_Call (Enclosing_Element (Identifier)) then
               Check_Casing (A4G_Bugs.Name_Image (Identifier), St_Casing_Keyword, Identifier);
            end if;
         end if;

         -- This procedure is also called on defining names for St_Casing, not interesting
         -- for St_Renamed_Entity
         if Rule_Used (St_Renamed_Entity) then
            Check_Renamed;
         end if;
      end if;

   end Process_Identifier;

   --------------------------
   -- Process__Association --
   --------------------------

   procedure Process_Association (Association : in Asis.Association) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Expressions, Asis.Elements, Asis.Statements;
      use Thick_Queries;

      procedure Check_Association (Na                  : Association_Names;
                                   Ident               : Asis.Element;
                                   Is_Positional       : Boolean;
                                   Associations_Length : Positive;
                                   Is_Operator         : Boolean := False)
      is
         use Named_Parameter_Flag_Utilities, Framework.Reports;
      begin
         if Association_Used (Na) and Is_Positional then
            if Na in Exceptionable_Association_Names then
               declare
                  Indicator : constant Root_Context'Class
                    := Matching_Context (Positional_Exceptions (Na), Ident, Extend_To => All_Extensions);
               begin
                  if Indicator /= No_Matching_Context then
                     return;
                  end if;
               end;
            end if;

            declare
               Ctx : constant Association_Context
                 := Association_Context (Corresponding_Context (St_Positional_Association, Image (Na)));
            begin
               if Is_Operator and Ctx.Except_Operator then
                  return;
               end if;
               if Associations_Length > Ctx.Allowed_Number then
                  Report (Rule_Id,
                          Ctx,
                          Get_Location (Association),
                          "positional association used in " & Image (Na, Lower_Case)
                          & Choose (Ctx.Allowed_Number = 0,
                              "",
                              " with more than " & Integer_Img (Ctx.Allowed_Number) & " element(s)"));
               end if;
            end;
         end if;
      end Check_Association;

      Encl   : Asis.Element;
      Called : Asis.Element;
   begin -- Process_Association
      if not Rule_Used (St_Positional_Association) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Encl := Enclosing_Element (Association);
      case Association_Kind (Association) is
         when Not_An_Association =>
            Failure ("Not an association", Association);
         when A_Discriminant_Association =>
            Check_Association (Na_Discriminant,
                               Nil_Element,
                               Is_Nil (Discriminant_Selector_Names (Association)),
                               Discriminant_Associations (Encl)'Length);
         when A_Record_Component_Association =>
            Check_Association (Na_Record_Aggregate,
                               Nil_Element,
                               Is_Nil (Record_Component_Choices (Association)),
                               Record_Component_Associations (Encl)'Length);
         when An_Array_Component_Association =>
            Check_Association (Na_Array_Aggregate,
                               Nil_Element,
                               Is_Nil (Array_Component_Choices (Association)),
                               Array_Component_Associations (Encl)'Length);
         when A_Pragma_Argument_Association =>
            Check_Association (Na_Pragma,
                               Encl,
                               Is_Nil (Formal_Parameter (Association)),
                               Pragma_Argument_Associations (Encl)'Length);
         when A_Parameter_Association =>
            -- Do not check infix (operators) function calls or attribute functions and procedures
            if Expression_Kind (Encl) = A_Function_Call then
               if Is_Prefix_Call (Encl) and then Expression_Kind (Prefix (Encl)) /= An_Attribute_Reference then
                  Check_Association (Na_Call,
                                     Called_Simple_Name (Encl),
                                     Is_Nil (Formal_Parameter (Association)),
                                     Function_Call_Parameters (Encl)'Length,
                                     Operator_Kind (Simple_Name (Prefix (Encl))) /= Not_An_Operator);
               end if;
            elsif Statement_Kind (Encl) = A_Procedure_Call_Statement then
               Called := Called_Simple_Name (Encl);
               if Expression_Kind (Called) /= An_Attribute_Reference then
                  Check_Association (Na_Call,
                                     Called,
                                     Is_Nil (Formal_Parameter (Association)),
                                     Call_Statement_Parameters (Encl)'Length);
               end if;
            else
               -- Entries, cannot be attributes...
               Called := Called_Simple_Name (Encl);
               if Expression_Kind (Called) = An_Indexed_Component then
                  -- Member of a family
                  Called := Prefix (Called);
               end if;
               Check_Association (Na_Call,
                                  Called,
                                  Is_Nil (Formal_Parameter (Association)),
                                  Call_Statement_Parameters (Encl)'Length);
            end if;
         when A_Generic_Association =>
            Check_Association (Na_Instantiation,
                               Generic_Unit_Name (Encl),
                               Is_Nil (Formal_Parameter (Association)),
                               Generic_Actual_Part (Encl)'Length);
      end case;
   end Process_Association;

   --------------------------------
   -- Process_Compound_Statement --
   --------------------------------

   procedure Process_Compound_Statement (Statement : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements, Asis.Text;
      use Framework.Reports;
      Elem_Span : Span;
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

      Elem_Span := Element_Span (Statement);
      if Elem_Span.Last_Line - Elem_Span.First_Line + 1 < Min_Stat_Length (Kind) then
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

      procedure Check_Formals (Subrule : St_Orders; Formals : Asis.Element_List; Message : Wide_String) is
         use Framework.Reports;
         State     : Order_Index := Order_Index'First;
         E_Mode    : Extended_Modes;
         Old_State : Order_Index;
         Enclosing : Asis.Declaration;
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
                  if Trait_Kind (Formal) = An_Access_Definition_Trait then
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
         if Rule_Used (St_Default_In) then
            for I in Formals'Range loop
               -- Note: Mode_Kind returns Not_A_Mode for generic formals that are not objects (types, subprograms...),
               --  so we don't need to special case these
               if Mode_Kind (Formals (I)) = A_Default_In_Mode
                 and then Trait_Kind (Formals (I)) /= An_Access_Definition_Trait  -- ASIS 95
                 and then Definition_Kind (Object_Declaration_View (Formals (I))) /= An_Access_Definition -- ASIS 2005
               then
                  Report (Rule_Id,
                          Corresponding_Context (St_Default_In),
                          Get_Location (Declaration_Subtype_Mark (Formals (I))),
                          "default IN mode used for " & Message);
               end if;
            end loop;
         end if;

         if (Rule_Used (St_Parameter_Order) or Rule_Used (St_Formal_Parameter_Order))
           and then Formals /= Nil_Element_List
         then
            Enclosing := Enclosing_Element (Formals (Formals'First));
           -- if it is a subprogram with an explicit spec, do not repeat message on body
            if Declaration_Kind (Enclosing) not in A_Procedure_Body_Declaration .. A_Function_Body_Declaration
                or else Is_Nil (Corresponding_Declaration (Enclosing))
            then
               for I in Formals'Range loop
                  Ignore := False;
                  case Declaration_Kind (Formals (I)) is
                     when A_Parameter_Specification =>
                        E_Mode  := Object_Mode (Formals (I));
                     when A_Formal_Object_Declaration =>
                        E_Mode  := Object_Mode (Formals (I));
                     when A_Formal_Type_Declaration =>
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
                        Failure ("style: inappropriate declaration_kind for formal", Formals (I));
                  end case;

                  if Rule_Used (Subrule) and not Ignore then
                     Old_State := State;
                     while not Mode_Order (Subrule) (State) (E_Mode) loop
                        if State = Order_Inx (Subrule) then
                           Report (Rule_Id,
                                   Corresponding_Context (Subrule),
                                   Get_Location (Formals (I)),
                                   "parameter out of order for " & Message);
                           -- Avoid multiple messages if there was only one parameter out of order:
                           State := Old_State;
                           exit;
                        end if;

                        State := State + 1;
                     end loop;
                  end if;
               end loop;
            end if;
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
           | A_Function_Declaration
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
            Check_Formals (St_Parameter_Order, Parameter_Profile (Declaration), "parameter specification");

         when A_Generic_Procedure_Declaration
           | A_Generic_Function_Declaration
              =>
            Check_Formals (St_Formal_Parameter_Order,
                           Generic_Formal_Part (Declaration),
                           "formal parameter declaration");
            Check_Formals (St_Parameter_Order,
                           Parameter_Profile (Declaration),
                           "parameter specification");

          when A_Generic_Package_Declaration =>
            Check_Formals (St_Formal_Parameter_Order,
                           Generic_Formal_Part (Declaration),
                           "formal parameter declaration");

         when others =>
            null;
      end case;
   end Process_Declaration;

   --------------------------
   -- Process_If_Statement --
   --------------------------

   procedure Process_If_Statement (Statement   : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Reports, Thick_Queries;
   begin
      if not Rule_Used (St_Negative_Condition) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Paths : constant Asis.Path_List := Statement_Paths (Statement);
         Func  : Asis.Expression;
      begin
         if Paths'Length = 2 and then Path_Kind (Paths (2)) = An_Else_Path then
            declare
               Expr : Asis.Expression := Condition_Expression (Paths (1));
            begin
               -- Get rid of possible spurious parentheses
               while Expression_Kind (Expr) = A_Parenthesized_Expression loop
                  Expr := Expression_Parenthesized (Expr);
               end loop;

               if Expression_Kind (Expr) = A_Function_Call then
                  Func := Simple_Name (Prefix (Expr));
                  if To_Upper (A4G_Bugs.Name_Image (Func)) = """NOT""" then
                     Report (Rule_Id,
                             Corresponding_Context (St_Negative_Condition),
                             Get_Location(Expr),
                             "Negative condition in ""if"" statement could be made positive");
                  end if;
               end if;
            end;
         end if;
      end;
   end Process_If_Statement;


   ---------------------
   -- Process_Literal --
   ---------------------

   procedure Process_Literal (Expression : in Asis.Expression) is
      use Asis.Expressions;

      -- Retrieve indices of main number formatters
      procedure Number_Decomposition (Name             : in     Wide_String;
                                      Base_Delimiter_1 :    out Natural;
                                      Base_Delimiter_2 :    out Natural;
                                      Dot_Delimiter    :    out Natural;
                                      Exp_Delimiter    :    out Natural)
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
                  Base_Delimiter_2 := Idx;
               else
                  Base_Delimiter_1 := Idx;
                  Is_Based_Literal := True;
               end if;
               Within_Based_Literal := not Within_Based_Literal;
            elsif Name (Idx) = '.' then
               Is_Real_Literal := True;
               Dot_Delimiter   := Idx;
            elsif (Name (Idx) = 'e' or else Name (Idx) = 'E') and then not Within_Based_Literal then
               Is_Exponentiated := True;
               Exp_Delimiter    := Idx;
            end if;
         end loop;
         -- Adjust values according to the kind of literal
         if not Is_Exponentiated then
            Exp_Delimiter := Name'Last + 1;
         end if;
         if not Is_Based_Literal then
            Base_Delimiter_1 := Name'First - 1;
            Base_Delimiter_2 := Exp_Delimiter;
         end if;
         if not Is_Real_Literal then
            Dot_Delimiter := Base_Delimiter_2;
         end if;
      end Number_Decomposition;


      -- Check separator positions according to the convention
      function Check_Separators (Name : in Wide_String; Block_Size : in Positive) return Boolean is
         Step   : constant Positive := Block_Size + 1;  -- The base step for separators
         Cursor : Positive          := 1;               -- The current tested character position
      begin
         for I in Name'Range loop
            -- Check if we should, or should not, match a separator
            if Cursor mod Step = 0 then
               -- We should match a separator
               if Name (I) /= '_' then
                  return False;
               end if;
            else
               -- We should not match a separator
               if Name (I) = '_' then
                  return False;
               end if;
            end if;
            Cursor := Cursor + 1;
         end loop;
         return True;
      end Check_Separators;


      procedure Process_Number_Separator is
         use Framework.Reports;

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

            Block_Size        : Positive;
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
               if not Check_Separators (Wide_String_Reverse (The_Integer_Part), Block_Size) or else
                 not Check_Separators (The_Decimal_Part, Block_Size)
               then
                  Report (Rule_Id,
                          Context,
                          Get_Location (Expression),
                          "number representation does not follow convention");
               end if;
            end if;
         end;
      end Process_Number_Separator;

      procedure Process_Exposed_Literal is
         use Framework.Reports, Literal_Flag_Utilities, Thick_Queries;
         use Asis, Asis.Elements, Asis.Text;

         function Normalize (S : Wide_String) return Wide_String is
            -- Get rid of initial spaces and surrounding quotes, change double double-quotes to single ones
            use Ada.Strings.Wide_Fixed;

            Result      : Wide_String (S'Range);
            Inx_Out     : Natural := S'First-1;
            Ignore_Next : Boolean := False;
         begin
            for I in Positive range Index (S, """")+1 .. S'Last-1 loop
               if Ignore_Next then
                  Ignore_Next := False;
               elsif S (I) = '"' then
                  Ignore_Next := True;
               else
                  Inx_Out := Inx_Out + 1;
                  Result (Inx_Out) := S (I);
               end if;
            end loop;
            return Result (Result'First .. Inx_Out);
         end Normalize;

         function Place return Place_Names is
         -- Search the place where the expression is used
            use Asis.Declarations;
            E        : Asis.Element := Expression;
            Top_Expr : Asis.Expression;
         begin
            loop
               case Element_Kind (E) is
                  when An_Expression | An_Association =>
                     Top_Expr := E;
                     E := Enclosing_Element (E);
                  when others =>
                     exit;
               end case;
            end loop;
            while Element_Kind (E) = A_Definition loop
               E := Enclosing_Element (E);
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
                           return Pl_Other;
                        end if;
                     when A_Type_Declaration |
                          A_Subtype_Declaration
                          =>
                        return Pl_Type;
                     when others =>
                        return Pl_Other;
                  end case;
               when A_Clause =>
                  case Clause_Kind (E) is
                     when A_Representation_Clause | A_Component_Clause =>
                        return Pl_Repr_Clause;
                     when others =>
                        return Pl_Other;
                  end case;
               when A_Pragma =>
                  return Pl_Pragma;
               when others =>
                  return Pl_Other;
            end case;
         end Place;

         Enclosing : Asis.Element;

      begin  -- Process_Exposed_Literal

         -- Note that if there is not check for the corresponding class of type,
         -- Report will be called with an Empty_Context (and thus will not report).
         case Expression_Kind (Expression) is
            when An_Integer_Literal =>
               if Permitted_Places (Lit_Integer) /= No_Place
                 and then Permitted_Places (Lit_Integer) (Place)
               then
                  -- Always allowed
                  return;
               end if;

               -- Find immediately enclosing expression, but get rid of spurious parentheses
               Enclosing := Enclosing_Element (Expression);
               while Expression_Kind (Enclosing) = A_Parenthesized_Expression loop
                  Enclosing := Enclosing_Element (Enclosing);
               end loop;

               if Permitted_Places (Lit_Integer) (Pl_Index)
                 and then Expression_Kind (Enclosing) = An_Indexed_Component
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
               declare
                  I : constant Extended_Biggest_Int := Extended_Biggest_Int'Wide_Value (Value_Image (Expression));
                  -- As a special exception, we use Extended_Biggest_Int instead of Biggest_Int here, because
                  -- it cannot be non-static (it is a litteral), and the user may well spell-out the full value
                  -- of System.Max_Int
               begin
                  for K in Permitted_Consts_Count range 1 .. Integer_Count loop
                     if Integer_Permitted_Values (K) = I then
                        -- OK just return
                        return;
                     end if;
                  end loop;
               end;

               Report (Rule_Id,
                       Corresponding_Context (St_Exposed_Literal, Image (Lit_Integer, Lower_Case)),
                       Get_Location (Expression),
                       "integer literal "
                       & Trim_All (Element_Image (Expression))
                       & " not in allowed construct");

            when A_Real_Literal =>
               if Permitted_Places (Lit_Real) /= No_Place
                 and then Permitted_Places (Lit_Real) (Place)
               then
                  -- Always allowed
                  return;
               end if;

               -- Compare to allowed values with a delta possible
               -- due to rounding conversion problems
               declare
                  F : constant Float := Float'Wide_Value (Value_Image (Expression));
               begin
                  for K in Permitted_Consts_Count range 1 .. Real_Count loop
                     if abs(Real_Permitted_Values (K)- F) < 2.0*Float'Model_Epsilon then
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
                          & Trim_All (Element_Image (Expression))
                          & " not in allowed construct");
               end;

            when A_Character_Literal =>
               if Permitted_Places (Lit_Character) /= No_Place
                 and then Permitted_Places (Lit_Character) (Place)
               then
                  -- Always allowed
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
               if Permitted_Places (Lit_String) /= No_Place
                 and then Permitted_Places (Lit_String) (Place)
               then
                  -- Always allowed
                  return;
               end if;

               -- Compare to allowed values
               declare
                  use String_Matching, Ada.Strings.Wide_Unbounded;
                  Good_Image : constant Wide_String := Normalize (Element_Image (Expression));
               begin
                  for I in Permitted_Consts_Count range 1 .. String_Count loop
                     if Match (Good_Image, To_Wide_String (String_Permitted_Values (I))) then
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
      if not (Rule_Used (St_Numeric_Literal) or Rule_Used (St_Exposed_Literal)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (St_Numeric_Literal)
        and then Expression_Kind (Expression) in An_Integer_Literal .. A_Real_Literal
      then
         Process_Number_Separator;
      end if;

      if Rule_Used (St_Exposed_Literal) then
         Process_Exposed_Literal;
      end if;
   end Process_Literal;

   ----------------------
   -- Process_Renaming --
   ----------------------

   procedure Process_Renaming  (Ren : in Asis.Declaration) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Framework.Reports;

      Target : Asis.Expression;
      Def    : Asis.Defining_Name;
   begin
      if not Rule_Used (St_Renamed_Entity) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Target := A4G_Bugs.Renamed_Entity (Ren);
      if Expression_Kind (Target) = A_Type_Conversion then
         Target := Converted_Or_Qualified_Expression (Target);
      end if;
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
            Renamed_Entities.Push ((Get_Location (Ren), Def));

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
      use Asis, Asis.Elements, Asis.Text;
      use Framework.Reports, Framework.Scope_Manager, Thick_Queries;
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
            for U in Use_Names'Range loop
               if Expression_Kind (Use_Names (U)) = A_Selected_Component then
                  Use_Def := Corresponding_Name_Definition (Selector (Use_Names (U)));
               else
                  Use_Def := Corresponding_Name_Definition (Use_Names (U));
               end if;

               Found := False;
               for W in With_Names'Range loop
                  if Expression_Kind (With_Names (W)) = A_Selected_Component then
                     With_Def := Corresponding_Name_Definition (Selector (With_Names (W)));
                  else
                     With_Def := Corresponding_Name_Definition (With_Names (W));
                  end if;

                  if Is_Equal (Use_Def, With_Def) then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Report (Rule_Id,
                          Corresponding_Context (St_Multiple_Elements, Image (Mu_Clause)),
                          Get_Location (Use_Names (U)),
                          "use clause does not start line and "
                          & Extended_Name_Image (Use_Names (U))
                          & " is not part of the preceding with clause");
               end if;
            end loop;
         end;
      end Check_Special_Use_Clause;

      Loc : Location;
   begin -- Process_Element
      if not Rule_Used (St_Multiple_Elements) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Special case:
      -- if Element is the declaration of a private compilation unit, consider it starts
      -- at the preceding "private"
      if Is_Compilation_Unit (Element)
        and then A4G_Bugs.Unit_Class (Enclosing_Compilation_Unit (Element)) = A_Private_Declaration
      then
         Loc := Get_Previous_Word_Location (Element, "PRIVATE");
      else
         Loc := Get_Location (Element);
      end if;

      declare
         First_Line               : constant Line_Number        := Get_First_Line (Loc);
         First_Column             : constant Character_Position := Get_First_Column (Loc);
         Element_Lines            : constant Line_List (1..1)   := Lines (Element, First_Line, First_Line);
         Element_First_Line_Image : constant Wide_String        := Line_Image (Element_Lines (Element_Lines'First));
      begin
         for I in Character_Position_Positive range 1 .. First_Column - 1 loop
            if Element_First_Line_Image (I) > ' ' then
               case Element_Kind (Element) is
                  when Not_An_Element =>
                     Failure (Rule_Id & ": Not_An_Element");

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
                     end if;

                  when A_Declaration =>
                     case Declaration_Kind (Element) is
                        when Not_A_Declaration =>
                           Failure (Rule_Id & ": Not_A_Declaration");
                        when An_Enumeration_Literal_Specification
                          | A_Discriminant_Specification
                          | A_Loop_Parameter_Specification
                          | A_Parameter_Specification
                          | An_Entry_Index_Specification
                          | A_Choice_Parameter_Specification
                          =>
                           -- These are allowed to appear on the same line as something else
                           null;
                        when others =>
                           Report
                             (Rule_Id,
                              Corresponding_Context (St_Multiple_Elements, Image (Mu_Declaration, Lower_Case)),
                              Loc,
                              "declaration does not start line");
                     end case;

                  when A_Statement =>
                     Report (Rule_Id,
                             Corresponding_Context (St_Multiple_Elements, Image (Mu_Statement, Lower_Case)),
                             Loc,
                             "statement does not start line");

                  when others =>
                     Failure (Rule_Id & ": inappropriate element kind");
               end case;
               return;
            end if;
         end loop;
      end;
   end Process_Element;

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
      Check_Casing (A4G_Bugs.Name_Image (Identifier), St_Casing_Attribute, Identifier);
   end Process_Attribute;

   --------------------
   -- Process_Pragma --
   --------------------

   procedure Process_Pragma (Pr : in Asis.Pragma_Element) is
      use Asis.Elements;
   begin
      if not Rule_Used (St_Casing_Pragma) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Check_Casing (Pragma_Name_Image (Pr), St_Casing_Pragma, Pr);
   end Process_Pragma;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Framework.Location) is
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
