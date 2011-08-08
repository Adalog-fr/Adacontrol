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

-- ASIS
with
  Asis.Elements,
  Asis.Declarations,
  Asis.Expressions,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  Framework,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
     Framework.Reports;

pragma Elaborate (Framework.Language);

package body Rules.Style is

   use Framework, Utilities;

   -- Possible parameters for the rule
   -- If new styles are added, all styles that have no parameters must stay together for convenience
   -- of Add_Use.
   type Style_Names is (St_Negative_Condition,  St_Default_In,
                        St_No_Closing_Name,     St_Positional_Association,
                        St_Casing, St_Literal, St_Multiple_Elements,
                        St_Exposed_Literal);    -- ABF 2006/07/31
   subtype No_Parameter_Styles is Style_Names range St_Negative_Condition .. St_Default_In;

   type Usage_Flags is array (Style_Names) of Boolean;

   package Style_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Style_Names,
                                                                          Prefix => "St_" );

   -- Parameters for the casing subrule
   type Casing_Names is (Ca_Uppercase, Ca_Lowercase, Ca_Titlecase, Ca_Original);
   package Casing_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Casing_Names,
                                                                           Prefix => "Ca_" );
   Casing_Policy : Casing_Names;
   -- We use a simple variable here rather than going through the context,
   -- because the rule can be given only once, and efficiency is a concern
   -- (the rule is called on every identifier).
   -- TBSL: case of count being different from search/check

   -- Parameters for the named_association subrule
   type Association_Names is (Na_Pragma, Na_Discriminant, Na_Call, Na_Instantiation, Na_Aggregate);

   type Association_Usage is array (Association_Names) of Boolean;
   package Named_Parameter_Flag_Utilities is new Framework.Language.Flag_Utilities (Flags => Association_Names,
                                                                                    Prefix => "Na_" );

   Rule_Used        : Usage_Flags  := (others => False);
   Save_Used        : Usage_Flags;
   Association_Used : Association_Usage := (others => False);

   type Closing_Name_Context is new Basic_Rule_Context with
      record
         Length : Asis.ASIS_Integer;
      end record;

   subtype Allowed_Bases is Positive range 2 .. 16;
   type Literal_Context is new Basic_Rule_Context with
      record
         Is_Not     : Boolean;
         Block_Size : Natural;
      end record;

   -- No more than Nbr_Of_Permitted_Consts constants of each type
   Nbr_Of_Permitted_Consts : constant := 20;

   -- Table for values given in the rule description
   Integer_Permitted_Values  : array (1 .. Nbr_Of_Permitted_Consts) of Integer;
   Float_Permitted_Values    : array (1 .. Nbr_Of_Permitted_Consts) of Float;
   Count_Integer             : Natural;     -- Number of Integer parameters
   Count_Float               : Natural;     -- Number of Float parameters

   Contexts : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      Style_Flag_Utilities.Help_On_Flags (Header => "Parameter(1): ");
      User_Message ("Parameter (2):   For no_closing_name   : maximum number of lines for allowing no closing name");
      User_Message ("                 For named_association : call      | Instantiation | Aggregate");
      User_Message ("                 For casing            : Uppercase | Lowercase     | Titlecase | Original");
      User_Message ("                                       (default = Original)");
      User_Message ("                 For element_span      : none");
      User_Message ("                 For literal           : [not] <base>");
      User_Message ("                 For exposed_literal   : <value>");
      User_Message ("Parameter (3):   For literal           : <block_size>");
      User_Message ("                 For exposed_literal   : <value>");
      User_Message ("Parameter (4..): For exposed_literal   : <value>");
      User_Message ("Control various Ada style issues");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use(Label : in Wide_String; Rule_Type : in Rule_Types) is
      use Framework.Language;
      use Casing_Flag_Utilities, Named_Parameter_Flag_Utilities, Style_Flag_Utilities;

      Style_Rule : Style_Names;
      Max_Length : Integer;
      Assoc      : Association_Names;
   begin
      if Parameter_Exists then
         Style_Rule := Get_Flag_Parameter (Allow_Any => False);
         case Style_Rule is
            when St_No_Closing_Name =>
               Rule_Used (St_No_Closing_Name) := True;
               if Parameter_Exists then
                  Max_Length := Get_Integer_Parameter;
               else
                  Max_Length := -1;
               end if;

               Associate (Contexts,
                          Value (Image (St_No_Closing_Name) & Rule_Types'Wide_Image (Rule_Type)),
                          Closing_Name_Context'(Basic.New_Context (Rule_Type, Label) with Max_Length));

            when St_Casing =>
               Rule_Used (St_Casing) := True;
               Associate (Contexts, Value (Image (St_Casing)), Basic.New_Context (Rule_Type, Label));
               if Parameter_Exists then
                  Casing_Policy := Get_Flag_Parameter (Allow_Any => False);
               else
                  Casing_Policy := Ca_Original;
               end if;

            when St_Positional_Association =>
               Rule_Used (St_Positional_Association) := True;
               if Parameter_Exists then
                  Assoc := Get_Flag_Parameter (Allow_Any => False);
                  Association_Used (Assoc) := True;
                  Associate (Contexts,
                             Value (Image (St_Positional_Association) & Image (Assoc)),
                             Basic.New_Context (Rule_Type, Label));
               else
                  Association_Used := (others => True);
                  for A in Association_Names loop
                     Associate (Contexts,
                                Value (Image (St_Positional_Association) & Image (A)),
                                Basic.New_Context (Rule_Type, Label));
                  end loop;
               end if;

            when St_Default_In
              | St_Negative_Condition
              =>
               Rule_Used (Style_Rule) := True;
               Associate (Contexts, Value (Image (Style_Rule)), Basic.New_Context (Rule_Type, Label));

            when St_Literal =>
               Rule_Used (St_Literal) := True;

               if not Parameter_Exists then
                  Parameter_Error (Rule_Id & ": invalid call to rule");
               end if;

               declare
                  -- Values needed for contexts
                  Is_Not     : constant Boolean := Get_Modifier ("NOT");
                  Base       : Allowed_Bases;
                  Block_Size : Natural;
                  -- Validation buffer
                  Buffer     : Integer;
               begin
                  -- Retrieve the user-defined base
                  Buffer := Get_Integer_Parameter;
                  if Buffer not in Allowed_Bases then
                     Parameter_Error (Rule_Id & ": base must be in range 2..16");
                  end if;
                  Base := Buffer;

                  -- Check for next parameters
                  if Is_Not then
                     if Parameter_Exists then
                        Parameter_Error (Rule_Id & ": cannot specify block size for unauthorized base");
                     end if;
                     Buffer := 0;
                  else
                     if not Parameter_Exists then
                        Parameter_Error (Rule_Id & ": block size needed for base");
                     end if;
                     Buffer := Get_Integer_Parameter;
                     if Buffer <= 0 then
                        Parameter_Error (Rule_Id & ": block size must be strictly positive");
                     end if;
                  end if;
                  Block_Size := Buffer;

                  Associate (Contexts,
                             Value (Image (St_Literal) & Allowed_Bases'Wide_Image (Base)),
                             Literal_Context'(Basic.New_Context (Rule_Type, Label) with Is_Not, Block_Size));
               end;

            when St_Exposed_Literal =>
               Rule_Used (St_Exposed_Literal) := True;

               if Parameter_Exists then
                  Count_Float   := 0;
                  Count_Integer := 0;
                  while Parameter_Exists loop
                     if Is_Integer_Parameter then
                        if Count_Integer = Nbr_Of_Permitted_Consts then
                           Parameter_Error (Rule_Id & ": too many integer values");
                        end if;

                        Count_Integer := Count_Integer + 1;
                        Integer_Permitted_Values (Count_Integer) := Get_Integer_Parameter;
                     elsif Is_Float_Parameter then
                        if Count_Float = Nbr_Of_Permitted_Consts then
                           Parameter_Error (Rule_Id & ": too many real values");
                        end if;

                        Count_Float := Count_Float + 1;
                        Float_Permitted_Values (Count_Float) := Get_Float_Parameter;
                     else
                        Parameter_Error (Rule_Id & ": integer or real value expected");
                     end if;
                  end loop;
               else
                  Count_Integer            := 2;
                  Integer_Permitted_Values := (0, 1, others=> 0);
                  Count_Float              := 2;
                  Float_Permitted_Values   := (0.0, 1.0, others=> 0.0);
               end if;
               Associate (Contexts, Value (Image (St_Exposed_Literal)), Basic.New_Context (Rule_Type, Label));

            when St_Multiple_Elements =>
               Rule_Used (St_Multiple_Elements) := True;
               if Parameter_Exists then
                  Parameter_Error (Rule_Id & ": this rule has no other parameter");
               end if;
               Associate (Contexts, Value (Image (St_Multiple_Elements)), Basic.New_Context (Rule_Type, Label));
         end case;

      else
         -- No parameter => all style checks
         Rule_Used := (others => True);

         for S in No_Parameter_Styles loop
            Associate (Contexts, Value (Image (S)), Basic.New_Context (Rule_Type, Label));
         end loop;

         -- Casing
         Associate (Contexts, Value (Image (St_Casing)), Basic.New_Context (Rule_Type, Label));
         Casing_Policy := Ca_Original;

         -- No_Closing_Name
         Associate (Contexts,
                    Value (Image (St_No_Closing_Name) & Rule_Types'Wide_Image (Rule_Type)),
                    Closing_Name_Context'(Basic.New_Context (Rule_Type, Label) with Length => -1));

         -- Positional_Association
         Association_Used := (others => True);
         for A in Association_Names loop
            Associate (Contexts,
                       Value (Image (St_Positional_Association) & Image (A)),
                       Basic.New_Context (Rule_Type, Label));
         end loop;

         -- Block_Number
         Associate (Contexts,
                    Value (Image (St_Literal) & Allowed_Bases'Wide_Image (10)),
                    Literal_Context'(Basic.New_Context (Rule_Type, Label)
                                                 with Is_Not => False, Block_Size => 3));

         -- Multiple_Elements
         Associate (Contexts, Value (Image (St_Multiple_Elements)), Basic.New_Context (Rule_Type, Label));

         -- St_Exposed_Literal
         Associate (Contexts, Value (Image (St_Exposed_Literal)), Basic.New_Context (Rule_Type, Label));
         Count_Integer            := 2;
         Integer_Permitted_Values := (0, 1, others=> 0);
         Count_Float              := 2;
         Float_Permitted_Values   := (0.0, 1.0, others=> 0.0);
       end if;
   exception
      when Already_In_Store =>
         Parameter_Error ("Parameter already provided for rule " & Rule_Id & " (" & Image (Style_Rule) & ')');
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
            Rule_Used := (others => False);
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
      Def_Name : Asis.Defining_Name;

      function Good_Name return Wide_String is
      begin
         if Element_Kind (Identifier) = A_Defining_Name then
            return Defining_Name_Image (Identifier);
         else
            return Name_Image (Identifier);
         end if;
      end Good_Name;
   begin
      if not Rule_Used (St_Casing) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Source_Name    : constant Wide_String := Good_Name;
         Reference_Name : Wide_String (Source_Name'Range);
         -- Note that the source name and the refence name always have the same length!
      begin
         case Casing_Policy is
            when Ca_Uppercase =>
               Reference_Name := To_Upper (Source_Name);
            when Ca_Lowercase =>
               Reference_Name := To_Lower (Source_Name);
            when Ca_Titlecase =>
               Reference_Name := To_Title (Source_Name);
            when Ca_Original =>
               if Element_Kind (Identifier) = A_Defining_Name then
                  -- Since it *is* the original...
                  return;
               end if;

               Def_Name := Corresponding_Name_Definition (Identifier);
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
                    Framework.Association (Contexts, Value (Image (St_Casing))),
                    Get_Location (Identifier),
                    "Wrong casing of """ & Source_Name & """, should be """ & Reference_Name & '"');
         end if;
      end;
   end Process_Identifier;

   --------------------------
   -- Process__Association --
   --------------------------

   procedure Process_Association (Association : in Asis.Association) is
      use Asis, Asis.Expressions, Asis.Elements, Asis.Statements;
      use Framework.Reports, Style_Flag_Utilities;

      procedure Check_Association (Na : Association_Names; Is_Positional : Boolean) is
         use Named_Parameter_Flag_Utilities;
      begin
         if (Association_Used = Association_Usage'(others => False) or Association_Used (Na))
           and then Is_Positional
         then
            Report(Rule_Id,
                   Framework.Association (Contexts, Value (Image (St_Positional_Association) & Image (Na))),
                   Get_Location(Association),
                   "named association not used in " & Image (Na));
         end if;
      end Check_Association;

   begin -- Process_Association

      if not Rule_Used (St_Positional_Association) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Association_Kind (Association) is
         when Not_An_Association =>
            Failure ("Not an association", Association);
         when A_Pragma_Argument_Association =>
            Check_Association (Na_Pragma, Is_Nil (Formal_Parameter (Association)));
         when A_Discriminant_Association =>
            Check_Association (Na_Discriminant, Is_Nil (Discriminant_Selector_Names (Association)));
         when A_Record_Component_Association =>
            Check_Association (Na_Aggregate, Is_Nil (Record_Component_Choices (Association)));
         when An_Array_Component_Association =>
            Check_Association (Na_Aggregate, Is_Nil (Array_Component_Choices (Association)));
         when A_Parameter_Association =>
            declare
               Encl : constant Asis.Element := Enclosing_Element (Association);
            begin
               -- Do not check infix (operators) function calls or attribute functions and procedures
               if Expression_Kind (Encl) = A_Function_Call then
                 if Is_Prefix_Call (Encl) and then Expression_Kind (Prefix (Encl)) /= An_Attribute_Reference then
                  Check_Association (Na_Call, Is_Nil (Formal_Parameter (Association)));
                 end if;
               elsif Statement_Kind (Encl) = A_Procedure_Call_Statement then
                  -- Entries cannot be attributes...
                  if Expression_Kind (Called_Name (Encl)) /= An_Attribute_Reference then
                     Check_Association (Na_Call, Is_Nil (Formal_Parameter (Association)));
                  end if;
               else
                  Check_Association (Na_Call, Is_Nil (Formal_Parameter (Association)));
               end if;
            end;
         when A_Generic_Association =>
            Check_Association (Na_Instantiation, Is_Nil (Formal_Parameter (Association)));
      end case;
   end Process_Association;

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


   --------------------
   -- Process_Number --
   --------------------

   procedure Process_Number (Expression : in Asis.Expression) is
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

      -- Reverse a Wide_String
      function Wide_String_Reverse (S : in Wide_String) return Wide_String is
         Result : Wide_String (S'Range);
      begin
         for Idx in Result'Range loop
            Result (Idx) := S (S'Last - Idx + S'First);
         end loop;
         return Result;
      end Wide_String_Reverse;

      use Framework.Reports;
      use Style_Flag_Utilities;

      procedure Process_Number_Separator is
         Name          : constant Wide_String := Value_Image (Expression);

         Base_Delimiter_1 : Integer;
         Base_Delimiter_2 : Integer;
         Dot_Delimiter    : Integer;
         Exp_Delimiter    : Integer;
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
                                  Association (Contexts, Value (Image (St_Literal) & " " & The_Base));

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
      begin
         declare
            use Asis, Asis.Elements;
            E : Asis.Element;
         begin
            -- Search if the expression is in constant by calling Enclosing_Element
            E := Expression;
            while Element_Kind (E) = An_Expression loop
               E := Enclosing_Element (E);
            end loop;

            -- If the expression Kind is a constant, authorized
            if Declaration_Kind (E) = A_Constant_Declaration
              or else Declaration_Kind (E) in A_Number_Declaration
            then
               -- OK just return
               return;
            end if;

            case Expression_Kind (Expression) is
               when An_Integer_Literal =>
                  -- Found something, compare to possible values
                  -- Conversion de name en value integer ?
                  declare
                     I : constant Integer := Integer'Wide_Value (Value_Image (Expression));
                  begin
                     for K in Integer range 1 .. Count_Integer loop
                        -- Element_Image est un Wide_String
                        if Integer_Permitted_Values (K) = I then
                           -- OK just return
                           return;
                        end if;
                     end loop;

                     Report (Rule_Id,
                             Framework.Association (Contexts, Value (Image (St_Exposed_Literal))),
                             Get_Location(Expression),
                             "integer value " & Value_Image (Expression) &" not allowed outside constant declaration");
                  end;

               when A_Real_Literal =>
                  -- Found something, compare to possible values with a delta possible
                  -- due to rounding conversion problems
                  declare
                     F : constant Float := Float'Wide_Value (Value_Image (Expression));
                  begin
                     for K in Integer range 1 .. Count_Float loop
                        if abs(Float_Permitted_Values (K)- F) < 2.0*Float'Model_Epsilon then
                           -- OK just return
                           return;
                        end if;
                     end loop;
                     -- After running in the permitted values, nothing found
                     -- Put a report
                     Report (Rule_Id,
                             Framework.Association (Contexts, Value (Image (St_Exposed_Literal))),
                             Get_Location(Expression),
                             "real value " & Value_Image (Expression) &" not allowed outside constant declaration");
                  end;

               when others =>
                  null;
            end case;
         end;
      end Process_Exposed_Literal;

   begin
      if not (Rule_Used (St_Literal) or Rule_Used (St_Exposed_Literal)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (St_Literal) then
         Process_Number_Separator;
      end if;

      if Rule_Used (St_Exposed_Literal) then
         Process_Exposed_Literal;
      end if;

   end Process_Number;

   ---------------------
   -- Process_Element --
   ---------------------

   procedure Process_Element (Element : in Asis.Element) is
      use Asis, Asis.Elements, Asis.Text;
      use Framework.Reports;
      use Style_Flag_Utilities;
   begin
      if not Rule_Used (St_Multiple_Elements) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Element_First_Line_Number : constant Line_Number      := First_Line_Number (Element);
         Element_Lines             : constant Line_List (1..1) :=
           Lines (Element, Element_First_Line_Number, Element_First_Line_Number);
         Element_First_Line_Image  : constant Wide_String      := Line_Image (Element_Lines (Element_Lines'First));

         The_Span : constant Span := Element_Span (Element);
      begin
         for I in Character_Position_Positive range 1 .. The_Span.First_Column - 1 loop
            if Element_First_Line_Image (I) > ' ' then
               case Element_Kind (Element) is
                  when Not_An_Element =>
                     Failure (Rule_Id & ": Not_An_Element");

                  when A_Clause =>
                           Report (Rule_Id,
                                   Framework.Association (Contexts, Value (Image (St_Multiple_Elements))),
                                   Get_Location (Element),
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
                           Report (Rule_Id,
                                   Framework.Association (Contexts, Value (Image (St_Multiple_Elements))),
                                   Get_Location (Element),
                                   "declaration does not start line");
                     end case;

                  when A_Statement =>
                     Report (Rule_Id,
                             Framework.Association (Contexts, Value (Image (St_Multiple_Elements))),
                             Get_Location (Element),
                             "statement does not start line");

                  when others =>
                     Failure (Rule_Id & ": inappropriate element kind");
               end case;
               return;
            end if;
         end loop;
      end;
   end Process_Element;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access,
                                              Prepare => Prepare'Access);
end Rules.Style;
