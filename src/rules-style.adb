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
  Asis.Elements,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Expressions,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  Framework,
  String_Matching,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;
pragma Elaborate (Framework.Language);

package body Rules.Style is

   use Framework, Utilities;

   -- Subrules for the rule
   -- "casing" subrules must stay together
   type Style_Names is (St_Casing_Attribute,   St_Casing_Identifier,      St_Casing_Pragma,
                        St_Compound_Statement, St_Default_In,             St_Exposed_Literal,
                        St_Multiple_Elements,  St_Negative_Condition,     St_No_Closing_Name,
                        St_Numeric_Literal,    St_Positional_Association, St_Renamed_Entity);
   subtype Casing_Styles is Style_Names range St_Casing_Attribute .. St_Casing_Pragma;
   type Usage_Flags is array (Style_Names) of Boolean;

   package Style_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Style_Names,
                                                                          Prefix => "St_" );
   --
   -- Parameters for the casing subrule
   --

   type Casing_Names is (Ca_Uppercase, Ca_Lowercase, Ca_Titlecase, Ca_Original);
   package Casing_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Casing_Names,
                                                                           Prefix => "Ca_" );
   Casing_Policy : array (Casing_Styles) of Casing_Names;
   -- We use a simple variable here rather than going through the context,
   -- because the rule can be given only once, and efficiency is a concern
   -- (the rule is called on every identifier).


   --
   -- Parameters for the compound_statement subrule
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
   -- Parameters for the exposed_literal subrule
   --

   type Literal_Names is (Lit_Integer, Lit_Real, Lit_Character, Lit_String);
   package Literal_Flag_Utilities  is new Framework.Language.Flag_Utilities (Flags => Literal_Names,
                                                                             Prefix => "Lit_" );
   type Place_Names is (Pl_Other, Pl_Constant, Pl_Number, Pl_Var_Init, Pl_Repr_Clause);
   -- Pl_Other used internally when not in one of the other Places, not accessible to user. Must stay first.
   type Place_Set is array (Place_Names) of Boolean;
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
   -- Parameters for the literal subrule
   --

   subtype Allowed_Bases is Positive range 2 .. 16;
   type Literal_Context is new Basic_Rule_Context with
      record
         Is_Not     : Boolean;
         Block_Size : Natural;
      end record;

   --
   -- Parameters for the multiple_elements subrule
   --

   type Multiple_Names is (Mu_Clause, Mu_Declaration, Mu_Statement);
   package Multiple_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags  => Multiple_Names,
                                                                             Prefix => "Mu_" );

   --
   -- Parameters for the no_closing_name subrule
   --

   type Closing_Name_Context is new Basic_Rule_Context with
      record
         Length : Asis.ASIS_Integer;
      end record;

   --
   -- Parameters for the positional_association subrule
   --

   type Association_Names is (Na_Pragma,        Na_Discriminant,    Na_Call,
                              Na_Instantiation, Na_Array_Aggregate, Na_Record_Aggregate);

   type Association_Usage is array (Association_Names) of Boolean;
   package Named_Parameter_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags  => Association_Names,
                                                                                    Prefix => "Na_" );
   Association_Used : Association_Usage := (others => False);

   type Association_Context is new Basic_Rule_Context with
      record
         Allowed_Number : Natural;
      end record;

   --
   -- Parameters for the renamed_entity subrule
   --

   type Renaming_Data is
      record
         Ren_Location : Location;
         Renamed_Def  : Asis.Defining_Name;
      end record;
   function Is_Same_Def (L, R : Renaming_Data) return Boolean;
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
      Style_Flag_Utilities.Help_On_Flags (Header => "Parameter(1):");

      User_Message ("For casing:");
      Casing_Flag_Utilities.Help_On_Flags (Header => "   Parameter (2):",
                                           Footer => "(default = Original)");

      User_Message ("For exposed_literal:");
      Literal_Flag_Utilities.Help_On_Flags (Header => "   Parameter (2):");
      User_Message ("   Parameter (3..): <value> | <place>");

      User_Message ("For multiple_elements:");
      Multiple_Flag_Utilities.Help_On_Flags (Header => "   Parameter (2..):",
                                                   Footer => "(default = all)");

      User_Message ("For no_closing_name:");
      User_Message ("   Parameter (2): maximum number of lines allowed");

      User_Message ("For numeric_literal:");
      User_Message ("   Parameter (2): [not] <base>");
      User_Message ("   Parameter (3): <block_size>");

      User_Message ("For positional_association:");
      Named_Parameter_Flag_Utilities.Help_On_Flags
        (Header => "   Parameter (2..):",
         Footer => "(default = all, each value may be followed by allowed number of occurrences)");

      User_Message ("Control various Ada style issues");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use(Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Framework.Language, Ada.Strings.Wide_Unbounded;
      use Casing_Flag_Utilities, Literal_Flag_Utilities, Multiple_Flag_Utilities, Named_Parameter_Flag_Utilities;
      use Place_Flag_Utilities, Style_Flag_Utilities;

      Subrule  : Style_Names;
      Max      : Integer;
      Assoc    : Association_Names;
      Multiple : Multiple_Names;
      Lit_Kind : Literal_Names;
      Places   : Place_Set := (others => False);
      P        : Place_Names;
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
                  Parameter_Error (Rule_Id, "subrule """ & Image (Subrule) & """ has no parameter");
               end if;
               Associate (Contexts, Value (Image (Subrule)), Basic.New_Context (Rule_Type, Label));
               Rule_Used (Subrule) := True;

            when Casing_Styles =>
               Associate (Contexts, Value (Image (Subrule)), Basic.New_Context (Rule_Type, Label));
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
                          Value (Image (St_Exposed_Literal) & Image (Lit_Kind)),
                          Basic.New_Context (Rule_Type, Label));
               Rule_Used (St_Exposed_Literal) := True;

            when St_Multiple_Elements =>
               if Parameter_Exists then
                  while Parameter_Exists loop
                     Multiple := Get_Flag_Parameter (Allow_Any => False);
                     Associate (Contexts,
                                Value (Image (St_Multiple_Elements) & Image (Multiple)),
                                Basic.New_Context (Rule_Type, Label));
                  end loop;
               else
                  for M in Multiple_Names loop
                     Associate (Contexts,
                                Value (Image (St_Multiple_Elements) & Image (M)),
                                Basic.New_Context (Rule_Type, Label));
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
                          Value (Image (St_No_Closing_Name) & Rule_Types'Wide_Image (Rule_Type)),
                          Closing_Name_Context'(Basic.New_Context (Rule_Type, Label) with Max));
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
                             Value (Image (St_Numeric_Literal) & Allowed_Bases'Wide_Image (Base)),
                             Literal_Context'(Basic.New_Context (Rule_Type, Label) with Is_Not, Block_Size));
                  Rule_Used (St_Numeric_Literal) := True;
              end;

            when St_Positional_Association =>
               if Parameter_Exists and then not Is_Integer_Parameter then
                  while Parameter_Exists loop
                     Assoc                    := Get_Flag_Parameter (Allow_Any => False);
                     Association_Used (Assoc) := True;

                     if Parameter_Exists and then Is_Integer_Parameter then
                        Max := Get_Integer_Parameter (Min => 0);
                     else
                        Max := 0;
                     end if;
                     Associate (Contexts,
                                Value (Image (St_Positional_Association) & Image (Assoc)),
                                Association_Context'(Basic.New_Context (Rule_Type, Label) with Max));
                  end loop;
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
                                Association_Context'(Basic.New_Context (Rule_Type, Label) with Max));
                  end loop;
               end if;
               Rule_Used (St_Positional_Association) := True;
         end case;

      else
         -- No parameter => all style checks
         Rule_Used := (others => True);

         -- Casing_Attribute
         Associate (Contexts, Value (Image (St_Casing_Attribute)), Basic.New_Context (Rule_Type, Label));
         Casing_Policy (St_Casing_Attribute) := Ca_Titlecase;

         -- Casing_Identifier
         Associate (Contexts, Value (Image (St_Casing_Identifier)), Basic.New_Context (Rule_Type, Label));
         Casing_Policy (St_Casing_Identifier) := Ca_Original;

         -- Casing_Pragma
         Associate (Contexts, Value (Image (St_Casing_Pragma)), Basic.New_Context (Rule_Type, Label));
         Casing_Policy (St_Casing_Identifier) := Ca_Titlecase;

         -- Compound_Statement
         Associate (Contexts, Value (Image (St_Compound_Statement)), Basic.New_Context (Rule_Type, Label));

         -- Default_In
         Associate (Contexts, Value (Image (St_Default_In)), Basic.New_Context (Rule_Type, Label));

         -- Exposed_Literal
         Associate (Contexts,
                    Value (Image (St_Exposed_Literal) & Image (Lit_Integer)),
                    Basic.New_Context (Rule_Type, Label));
         Integer_Count            := 2;
         Integer_Permitted_Values := (0, 1, others=> 0);

         Associate (Contexts,
                    Value (Image (St_Exposed_Literal) & Image (Lit_Real)),
                    Basic.New_Context (Rule_Type, Label));
         Real_Count            := 2;
         Real_Permitted_Values := (0.0, 1.0, others => 0.0);
         Permitted_Places      := (others => (Pl_Constant | Pl_Number => True, others => False));

         -- Multiple_Elements
         for M in Multiple_Names loop
            Associate (Contexts,
                       Value (Image (St_Multiple_Elements) & Image (M)),
                       Basic.New_Context (Rule_Type, Label));
         end loop;

         -- Negative_Condition
         Associate (Contexts, Value (Image (St_Negative_Condition)), Basic.New_Context (Rule_Type, Label));

         -- No_Closing_Name
         Associate (Contexts,
                    Value (Image (St_No_Closing_Name) & Rule_Types'Wide_Image (Rule_Type)),
                    Closing_Name_Context'(Basic.New_Context (Rule_Type, Label) with Length => -1));

         -- Numeric_Literal
         Associate (Contexts,
                    Value (Image (St_Numeric_Literal) & Allowed_Bases'Wide_Image (10)),
                    Literal_Context'(Basic.New_Context (Rule_Type, Label)
                                                 with Is_Not => False, Block_Size => 3));

         -- Positional_Association
         Association_Used := (others => True);
         for A in Association_Names loop
            Associate (Contexts,
                       Value (Image (St_Positional_Association) & Image (A)),
                       Association_Context'(Basic.New_Context (Rule_Type, Label) with 0));
         end loop;

       end if;
   exception
      when Already_In_Store =>
         Parameter_Error (Rule_Id, "Subrule already provided: " & Image (Subrule));
   end Add_Use;

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
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -----------------
   -- Is_Same_Def --
   -----------------

   function Is_Same_Def (L, R : Renaming_Data) return Boolean is
      use Asis.Elements;
   begin
      return Is_Equal (L.Renamed_Def, R.Renamed_Def);
   end Is_Same_Def;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      -- Make defaults for unspecified Check/Search/Count for style No_Closing_Name
      use Style_Flag_Utilities;
   begin
      for R in Rule_Types loop
         if Association (Contexts, Value (Image (St_No_Closing_Name) & Rule_Types'Wide_Image (R)))
           = No_Matching_Context
         then
               Associate (Contexts,
                          Value (Image (St_No_Closing_Name) & Rule_Types'Wide_Image (R)),
                          Closing_Name_Context'(Basic.New_Context (R, "") with Asis.ASIS_Integer'Last));
         end if;
      end loop;
   end Prepare;

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
      use Framework.Reports, Style_Flag_Utilities;

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

            if Defining_Name_Kind (Def_Name) = A_Defining_Operator_Symbol then
               -- This should follow the rules for keywords (not checked currently),
               -- not the original
               return;
            end if;

            Reference_Name := Defining_Name_Image (Def_Name);
      end case;

      if Source_Name /= Reference_Name then
         Report (Rule_Id,
                 Framework.Association (Contexts, Value (Image (Casing))),
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
      use Asis.Declarations, Asis.Text;
      use Framework.Reports, Style_Flag_Utilities;
      Length : Line_Number;
   begin
      Rules_Manager.Enter (Rule_Id);
      if not Rule_Used (St_No_Closing_Name) then
         return ;
      end if;

      if not Is_Name_Repeated (Construct) then
         Length := Last_Line_Number (Construct) - First_Line_Number (Construct) + 1;
         if Length > Closing_Name_Context (Association (Contexts,
                                                        Value (Image (St_No_Closing_Name)
                                                               & Rule_Types'Wide_Image (Check)))).Length
         then
            Report (Rule_Id,
                    Association (Contexts, Value (Image (St_No_Closing_Name) & Rule_Types'Wide_Image (Check))),
                    Get_Location (Construct),
                    "name not repeated at the end");
         elsif Length > Closing_Name_Context (Association (Contexts,
                                                           Value (Image (St_No_Closing_Name)
                                                                  & Rule_Types'Wide_Image (Search)))).Length
         then
            Report (Rule_Id,
                    Association (Contexts, Value (Image (St_No_Closing_Name)
                                                  & Rule_Types'Wide_Image (Search))),
                    Get_Location (Construct),
                    "name not repeated at the end");
         end if;
         if Length > Closing_Name_Context (Association (Contexts,
                                                        Value (Image (St_No_Closing_Name)
                                                               & Rule_Types'Wide_Image (Count)))).Length
         then
            Report (Rule_Id,
                    Association (Contexts, Value (Image (St_No_Closing_Name) & Rule_Types'Wide_Image (Count))),
                    Get_Location (Construct),
                    "name not repeated at the end");
         end if;
      end if;

   end Process_Construct;

   ------------------------
   -- Process_Identifier --
   ------------------------

   procedure Process_Identifier (Identifier : in Asis.Expression) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Declarations, Framework.Reports, Style_Flag_Utilities;

    procedure Check_Renamed is
         use Framework.Scope_Manager;

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
                 Framework.Association (Contexts, Value (Image (St_Renamed_Entity))),
                 Get_Location (Identifier),
                 Defining_Name_Image (Def) & " has been renamed at " & Image (Ren.Ren_Location));
      end Check_Renamed;

   begin  -- Process_Identifier
      if not (Rule_Used (St_Casing_Identifier) or Rule_Used (St_Renamed_Entity)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (St_Casing_Identifier) then
         if Element_Kind (Identifier) = A_Defining_Name then
            Check_Casing (Defining_Name_Image (Identifier), St_Casing_Identifier, Identifier);
         else
            Check_Casing (Name_Image (Identifier), St_Casing_Identifier, Identifier);
         end if;
      end if;

      -- This procedure is also called on defining names for St_Casing, not interesting
      -- for St_Renamed_Entity
      if Rule_Used (St_Renamed_Entity) and then Element_Kind (Identifier) /= A_Defining_Name then
         Check_Renamed;
      end if;
   end Process_Identifier;

   --------------------------
   -- Process__Association --
   --------------------------

   procedure Process_Association (Association : in Asis.Association) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Expressions, Asis.Elements, Asis.Statements;
      use Framework.Reports, Style_Flag_Utilities;

      procedure Check_Association (Na                  : Association_Names;
                                   Is_Positional       : Boolean;
                                   Associations_Length : Positive)
      is
         use Named_Parameter_Flag_Utilities;
      begin
         if Association_Used (Na) and then Is_Positional then
            declare
               Ctx : constant Association_Context
                 := Association_Context (Framework.Association
                                         (Contexts,
                                          Value (Image (St_Positional_Association) & Image (Na))));
            begin
               if Associations_Length > Ctx.Allowed_Number then
                  Report (Rule_Id,
                          Ctx,
                          Get_Location (Association),
                          "positional association used in " & Image (Na)
                    & Choose (Ctx.Allowed_Number = 0,
                              "",
                              " with more than " & Integer_Img (Ctx.Allowed_Number) & " element(s)"));
               end if;
            end;
         end if;
      end Check_Association;

      Encl : Asis.Element;
   begin -- Process_Association
      if not Rule_Used (St_Positional_Association) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Encl := Enclosing_Element (Association);
      case Association_Kind (Association) is
         when Not_An_Association =>
            Failure ("Not an association", Association);
         when A_Pragma_Argument_Association =>
            Check_Association (Na_Pragma,
                               Is_Nil (Formal_Parameter (Association)),
                               Pragma_Argument_Associations (Encl)'Length);
         when A_Discriminant_Association =>
            Check_Association (Na_Discriminant,
                               Is_Nil (Discriminant_Selector_Names (Association)),
                               Discriminant_Associations (Encl)'Length);
         when A_Record_Component_Association =>
            Check_Association (Na_Record_Aggregate,
                               Is_Nil (Record_Component_Choices (Association)),
                               Record_Component_Associations (Encl)'Length);
         when An_Array_Component_Association =>
            Check_Association (Na_Array_Aggregate,
                               Is_Nil (Array_Component_Choices (Association)),
                               Array_Component_Associations (Encl)'Length);
         when A_Parameter_Association =>
            Encl := Enclosing_Element (Association);
            -- Do not check infix (operators) function calls or attribute functions and procedures
            if Expression_Kind (Encl) = A_Function_Call then
               if Is_Prefix_Call (Encl) and then Expression_Kind (Prefix (Encl)) /= An_Attribute_Reference then
                  Check_Association (Na_Call,
                                     Is_Nil (Formal_Parameter (Association)),
                                     Function_Call_Parameters (Encl)'Length);
               end if;
            elsif Statement_Kind (Encl) = A_Procedure_Call_Statement then
               -- Entries cannot be attributes...
               if Expression_Kind (Called_Name (Encl)) /= An_Attribute_Reference then
                  Check_Association (Na_Call,
                                     Is_Nil (Formal_Parameter (Association)),
                                     Call_Statement_Parameters (Encl)'Length);
               end if;
            else
               Check_Association (Na_Call,
                                  Is_Nil (Formal_Parameter (Association)),
                                  Call_Statement_Parameters (Encl)'Length);
            end if;
         when A_Generic_Association =>
            Check_Association (Na_Instantiation,
                               Is_Nil (Formal_Parameter (Association)),
                               Generic_Actual_Part (Encl)'Length);
      end case;
   end Process_Association;

   --------------------------------
   -- Process_Compound_Statement --
   --------------------------------

   procedure Process_Compound_Statement (Statement : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Statements, Asis.Text;
      use Framework.Reports, Style_Flag_Utilities;
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
                 Framework.Association (Contexts, Value (Image (St_Compound_Statement))),
                 Get_Location (Statement),
                 "Statement has less than" & Line_Number'Wide_Image (Min_Stat_Length (Kind)) & " lines");
      end if;
   end Process_Compound_Statement;

   -------------------------
   -- Process_Declaration --
   -------------------------

   -- Checking that all mode parameter are explicit (no default in)
   --
   -- It just uses the Mode_Kind function provides by ASIS
   -- on parameter specification get from procedure declaration

   procedure Process_Declaration (Declaration: in Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;

      procedure Check (Formals : Asis.Element_List; Message : Wide_String) is
         use Framework.Reports, Style_Flag_Utilities;
      begin
         for I in Formals'Range loop
            if Mode_Kind (Formals (I)) = A_Default_In_Mode then
               Report (Rule_Id,
                       Framework.Association (Contexts, Value (Image (St_Default_In))),
                       Get_Location(Formals (I)),
                       "default IN mode used for " & Message);
            end if;
         end loop;
      end Check;

   begin
      if not Rule_Used (St_Default_In) then
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
            Check (Parameter_Profile(Declaration), "parameter specification");

         when A_Generic_Procedure_Declaration
           | A_Generic_Function_Declaration
           =>
            Check (Parameter_Profile  (Declaration), "parameter specification");
            Check (Generic_Formal_Part(Declaration), "formal object declaration");

          when A_Generic_Package_Declaration =>
            Check (Generic_Formal_Part(Declaration), "formal object declaration");

         when others =>
            null;
      end case;
   end Process_Declaration;

   --------------------------
   -- Process_If_Statement --
   --------------------------

   procedure Process_If_Statement (Statement   : in Asis.Statement) is
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Framework.Reports, Style_Flag_Utilities;
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
                  Func := Prefix (Expr);
                  if Expression_Kind (Func) = A_Selected_Component then
                     Func := Selector (Func);
                  end if;
                  if To_Upper (Name_Image (Func)) = """NOT""" then
                     Report (Rule_Id,
                             Framework.Association (Contexts, Value (Image (St_Negative_Condition))),
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
         use Framework.Reports, Style_Flag_Utilities;

         Name          : constant Wide_String := Value_Image (Expression);

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
      begin

         Number_Decomposition (Name, Base_Delimiter_1, Base_Delimiter_2, Dot_Delimiter, Exp_Delimiter);
         declare
            The_Base_Part     : constant Wide_String := Name (Name'First           .. Base_Delimiter_1 - 1);
            The_Integer_Part  : constant Wide_String := Name (Base_Delimiter_1 + 1 .. Dot_Delimiter    - 1);
            The_Decimal_Part  : constant Wide_String := Name (Dot_Delimiter    + 1 .. Base_Delimiter_2 - 1);

            The_Base          : constant Wide_String        := Utilities.Choose (Condition  => The_Base_Part = "",
                                                                                 When_True  => "10",
                                                                                 When_False => The_Base_Part);
            Context           : constant Root_Context'Class :=
                                  Association (Contexts, Value (Image (St_Numeric_Literal) & " " & The_Base));

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
         use Ada.Strings, Ada.Strings.Wide_Fixed, Framework.Reports, Literal_Flag_Utilities, Style_Flag_Utilities;
         use Asis, Asis.Declarations, Asis.Elements, Asis.Text;

         function Normalize (S : Wide_String) return Wide_String is
            -- Get rid of initial spaces and surrounding quotes, change double double-quotes to single ones
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

         E        : Asis.Element;
         Top_Expr : Asis.Expression;
         Place    : Place_Names;
      begin
         -- Search the place where the expression is used
         E  := Expression;
         loop
            case Element_Kind (E) is
               when An_Expression | An_Association =>
                  Top_Expr := E;
                  E := Enclosing_Element (E);
               when others =>
                  exit;
            end case;
         end loop;
         if Element_Kind (E) = A_Definition then
            E := Enclosing_Element (E);
         end if;

         case Element_Kind (E) is
            when A_Declaration =>
               case Declaration_Kind (E) is
                  when A_Constant_Declaration =>
                     Place := Pl_Constant;
                  when A_Number_Declaration =>
                     Place := Pl_Number;
                  when A_Variable_Declaration =>
                     if Is_Equal (Top_Expr, Initialization_Expression (E)) then
                        Place := Pl_Var_Init;
                     else
                        Place := Pl_Other;
                     end if;
                  when others =>
                     Place := Pl_Other;
               end case;
            when A_Clause =>
               case Clause_Kind (E) is
                  when A_Representation_Clause | A_Component_Clause =>
                     Place := Pl_Repr_Clause;
                  when others =>
                     Place := Pl_Other;
               end case;
            when others =>
               Place := Pl_Other;
         end case;

         -- Now check if allowed
         -- Note that if there is not check for the corresponding class of type,
         -- Report will be called with an Empty_Context (and thus will not report).
         case Expression_Kind (Expression) is
            when An_Integer_Literal =>
               if Permitted_Places (Lit_Integer) (Place) then
                  -- Always allowed
                  return;
               end if;

               -- Compare to allowed values
               declare
                  use Thick_Queries;
                  I : constant Biggest_Int := Biggest_Int'Wide_Value (Value_Image (Expression));
               begin
                  for K in Permitted_Consts_Count range 1 .. Integer_Count loop
                     if Integer_Permitted_Values (K) = I then
                        -- OK just return
                        return;
                     end if;
                  end loop;

                  Report (Rule_Id,
                    Framework.Association (Contexts,
                      Value (Image (St_Exposed_Literal) & Image (Lit_Integer))),
                    Get_Location(Expression),
                    "integer literal "
                    & Trim_All (Element_Image (Expression))
                    & " not in allowed construct");
               end;

            when A_Real_Literal =>
               if Permitted_Places (Lit_Real) (Place) then
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
                    Framework.Association (Contexts,
                      Value (Image (St_Exposed_Literal) & Image (Lit_Real))),
                    Get_Location(Expression),
                    "real literal "
                    & Trim_All (Element_Image (Expression))
                    & " not in allowed construct");
               end;

            when A_Character_Literal =>
               if Permitted_Places (Lit_Character) (Place) then
                  -- Always allowed
                  return;
               end if;

               Report (Rule_Id,
                 Framework.Association (Contexts,
                   Value (Image (St_Exposed_Literal) & Image (Lit_Character))),
                 Get_Location(Expression),
                 "character literal "
                 & Trim_All (Element_Image (Expression))
                 & " not in allowed construct");

            when A_String_Literal =>
               if Permitted_Places (Lit_String) (Place) then
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
                    Framework.Association (Contexts,
                      Value (Image (St_Exposed_Literal) & Image (Lit_String))),
                    Get_Location(Expression),
                    "string literal "
                    & Trim_All (Element_Image (Expression))
                    & " not in allowed construct");
               end;

            when others =>
               Failure ("Unexpected literal");
         end case;

      end Process_Exposed_Literal;

      use Asis, Asis.Elements;
   begin
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
            case Declaration_Kind (Ren) is
               when An_Object_Renaming_Declaration
                  | An_Exception_Renaming_Declaration
                    =>
                  Renamed_Entities.Push ((Get_Location (Ren), Def));
               when A_Package_Renaming_Declaration .. A_Generic_Function_Declaration =>
                  Renamed_Entities.Push_Enclosing ((Get_Location (Ren), Def));
               when others =>
                  Failure ("not a renaming declaration");
            end case;
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
      use Framework.Reports, Thick_Queries;
      use Style_Flag_Utilities, Multiple_Flag_Utilities;
      Loc : Location;
   begin
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
         Loc := Get_Previous_Word_Location (Element);
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
                     Report (Rule_Id,
                             Framework.Association (Contexts, Value (Image (St_Multiple_Elements) & Image (Mu_Clause))),
                             Loc,
                             "clause does not start line");

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
                              Framework.Association (Contexts,
                                                     Value (Image (St_Multiple_Elements) & Image (Mu_Declaration))),
                              Loc,
                              "declaration does not start line");
                     end case;

                  when A_Statement =>
                     Report (Rule_Id,
                       Framework.Association (Contexts,
                                              Value (Image (St_Multiple_Elements) & Image (Mu_Statement))),
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

      Identifier := A4G_Bugs.Attribute_Designator_Identifier (Attribute);
      Check_Casing (Name_Image (Identifier), St_Casing_Attribute, Identifier);
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

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access,
                                              Prepare => Prepare'Access);
end Rules.Style;
