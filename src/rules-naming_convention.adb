----------------------------------------------------------------------
--  Rules.Naming_Convention - Package body                          --
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
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  String_Matching,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;


package body Rules.Naming_Convention is
   use Framework, Ada.Strings.Wide_Unbounded;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   -- Here are all keywords for classifying naming conventions
   -- To add a new one, just add it to the type Keys, and insert it at the appropriate places
   -- in the calls to Check in the Process_XXX procedures.
   -- Presentation reflects the hierarchy of naming conventions
   type Keys is (K_All,
                    K_Type,
                       K_Discrete_Type,
                          K_Enumeration_Type,
                          K_Integer_Type,
                             K_Signed_Integer_Type,
                             K_Modular_Integer_Type,
                          K_Floating_Point_Type,
                          K_Fixed_Point_Type,
                             K_Binary_Fixed_Point_Type,
                             K_Decimal_Fixed_Point_Type,
                       K_Array_Type,
                       K_Record_Type,
                          K_Regular_Record_Type,
                          K_Tagged_Type,
                          K_Class_Type,
                       K_Access_Type,
                           K_Access_To_Regular_Type,
                           K_Access_To_Tagged_Type,
                           K_Access_To_Class_Type,
                           K_Access_To_SP_Type,
                           K_Access_To_Task_Type,
                           K_Access_To_Protected_Type,
                      K_Private_Type,
                          K_Private_Extension,
                       K_Generic_Formal_Type,
                    K_Variable,
                       K_Regular_Variable,
                       K_Field,
                          K_Discriminant,
                          K_Record_Field,
                          K_Protected_Field,
                       K_Procedure_Formal_Out,
                       K_Procedure_Formal_In_Out,
                       K_Generic_Formal_In_Out,
                    K_Constant,
                       K_Regular_Constant,
                       K_Named_Number,
                          K_Integer_Number,
                          K_Real_Number,
                       K_Enumeration,
                       K_Sp_Formal_In,
                       K_Generic_Formal_In,
                       K_Loop_Control,
                       K_Occurrence_Name,
                       K_Entry_Index,
                    K_Label,
                    K_Stmt_Name,
                       K_Loop_Name,
                       K_Block_Name,
                    K_Subprogram,
                       K_Procedure,
                          K_Regular_Procedure,
                          K_Protected_Procedure,
                          K_Generic_Formal_Procedure,
                       K_Function,
                          K_Regular_Function,
                          K_Protected_Function,
                          K_Generic_Formal_Function,
                       K_Entry,
                          K_Task_Entry,
                          K_Protected_Entry,
                   K_Package,
                       K_Regular_Package,
                       K_Generic_Formal_Package,
                    K_Task,
                       K_Task_Type,
                       K_Task_Object,
                    K_Protected,
                       K_Protected_Type,
                       K_Protected_Object,
                    K_Exception,
                    K_Generic,
                       K_Generic_Package,
                       K_Generic_Sp,
                          K_Generic_Procedure,
                          K_Generic_Function
                );
   type Key_Set_Index is range 1 .. Keys'Pos (Keys'Last) + 1;
   type Key_Set is array (Key_Set_Index range <>) of Keys;

   type Usage_Rec;
   type Usage_Rec_Access is access Usage_Rec;
   type Pattern_Access is access String_Matching.Compiled_Pattern;
   type Usage_Rec is new Simple_Context with
      record
         Is_Not  : Boolean;
         Pattern : Pattern_Access;
         Next    : Usage_Rec_Access;
      end record;

   type Rule_Rec is
      record
         Is_Root : Boolean := False;
         First   : Usage_Rec_Access;
      end record;
   Usage : array (Keys) of Rule_Rec;

   -----------
   -- Clear --
   -----------

   procedure Clear (Rec : in out Usage_Rec_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Usage_Rec, Usage_Rec_Access);
      procedure Free is new Ada.Unchecked_Deallocation (String_Matching.Compiled_Pattern, Pattern_Access);
      Temp : Usage_Rec_Access := Rec;
   begin
      while Rec /= null loop
         Temp := Rec.Next;
         Free (Rec.Pattern);
         Free (Rec);
         Rec := Temp;
      end loop;
   end Clear;

   -----------
   -- Image --
   -----------

   function Image (Key : Keys) return Wide_String is
      use Utilities;
      Img : constant Wide_String := To_Lower (Keys'Wide_Image (Key));
   begin
      -- Remove "K_"
      return Img (3 .. Img'Last);
   end Image;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
      procedure Print_Kw is
         Header : constant Wide_String := "Parameter 1: ";
         use Ada.Strings.Wide_Fixed;

         Spacing : constant Natural     := Keys'Wide_Width - 2 + 1;
         Line    : Wide_String (1..79)  := (others => ' ');
         Pos     : Natural;
      begin
         Overwrite (Line, 1, Header);
         Pos := Header'Length + 1;
         Overwrite (Line, Pos, Image (Keys'First));
         Pos := Pos + Spacing;

         for Value in Keys range Keys'Succ(Keys'First) .. Keys'Last loop
            Overwrite (Line, Pos, "| " & Image (Value));
            Pos := Pos +  2 + Spacing;
            if Pos + Keys'Wide_Width -1 > Line'Last then
               User_Message (Line (1 .. Pos-2));
               Line := (others => ' ');
               Pos := Header'Length + 1 - 2;
           end if;
         end loop;

         if Line /= (Line'range => ' ') then
            User_Message (Line (Line'First .. Pos-1));
         end if;
      end Print_Kw;
   begin
      User_Message ("Rule: " & Rule_Id);
      Print_Kw;
      User_Message ("Parameter 2..N: [not] [case_sensitive|case_insensitive] ""<name pattern>""");
      User_Message ("Control the form of allowed (or forbidden) names in declarations");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language, String_Matching;

      function Get_Key_Parameter is new Get_Flag_Parameter (Flags     => Keys,
                                                            Allow_Any => False,
                                                            Prefix    => "K_");
      Key     : Keys;
      Is_Root : Boolean;
   begin
      if not Parameter_Exists then
         Parameter_Error ("Kind of filter required for rule " & Rule_Id);
      end if;
      Is_Root := Get_Modifier ("ROOT");
      Key     := Get_Key_Parameter;

      if not Parameter_Exists then
         Parameter_Error ("At least one pattern required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         declare
            Ignore_Case : constant Boolean     := Get_Modifier (True_KW  => "CASE_INSENSITIVE",
                                                                False_KW => "CASE_SENSITIVE",
                                                                Default => True);
            Is_Not      : constant Boolean     := Get_Modifier ("NOT");
            Pattern     : constant Wide_String := Get_String_Parameter;
         begin
            Usage (Key) := (Is_Root => Usage (Key).Is_Root or Is_Root,
                            First   => new Usage_Rec'(Rule_Type,
                                                      To_Unbounded_Wide_String (Label),
                                                      Is_Not  => Is_Not,
                                                      Pattern => new Compiled_Pattern'(Compile (Pattern, Ignore_Case)),
                                                      Next    => Usage (Key).First));
         exception
            when Pattern_Error =>
               Parameter_Error ("Incorrect pattern: " & Pattern);
         end;
      end loop;

      Rule_Used := True;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            for I in Usage'Range loop
               Clear (Usage (I).First);
               Usage (I).Is_Root := False;
            end loop;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Name : in Asis.Defining_Name) is
      use Utilities, Thick_Queries, String_Matching,
        Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions,
        Asis.Statements, Framework.Reports;

      procedure Check_One (Name_Str : Wide_String; Key : Keys) is
         Current              : Usage_Rec_Access := Usage (Key).First;
         Matches              : Boolean;
         All_Not_Patterns     : Boolean := True;
         Positive_Match_Found : Boolean := False;
      begin
         if Current = null then
            -- No rule
            return;
         end if;

         loop
            Matches := Match (Name_Str, Current.Pattern.all);
            if Matches then
               if Current.Is_Not then
                  Report (Rule_Id,
                          To_Wide_String (Current.Rule_Label),
                          Current.Rule_Type,
                          Get_Location (Name),
                          "Name does not follow naming rule for """
                          & Image (Key)
                          & """: """
                          & Defining_Name_Image (Name)& '"');
                  return;
               else
                  -- We must continue in case there is a "not" match farther
                  Positive_Match_Found := True;
               end if;
            elsif not Current.Is_Not then
               All_Not_Patterns := False;
            end if;

            exit when Current.Next = null;
            Current  := Current.Next;
         end loop;

         if Positive_Match_Found then
            return;

         -- No match found here. It is an error, unless all patterns were "not" patterns.
         -- Current points to the last rule, which is the first one specified
         -- since we chain on head.
         elsif not All_Not_Patterns then
            Report (Rule_Id,
                    To_Wide_String (Current.Rule_Label),
                    Current.Rule_Type,
                    Get_Location (Name),
                    "Name does not follow naming rule for """
                    & Image (Key)
                    & """: """
                    & Defining_Name_Image (Name)& '"');
         end if;
      end Check_One;

   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Decl      : Asis.Declaration     := Enclosing_Element (Name);
         Name_Str  : constant Wide_String := Defining_Name_Image (Name);
         Renamed   : Asis.Element;
         Decl_Kind : Asis.Declaration_Kinds;
         Def       : Asis.Definition;
         Accessed  : Asis.Element;

         -- Applicable rules must be given in order of decreasing generality
         procedure Check (Set : Key_Set) is
         begin
            for I in reverse Set'Range loop
               Check_One (Name_Str, Set (I));
               exit when Usage (Set (I)).Is_Root;
            end loop;
         end Check;
      begin
         if Defining_Name_Kind (Decl) = A_Defining_Expanded_Name then
            -- Name was the name of a child compilation unit
            Decl := Enclosing_Element (Decl);
         end if;

         case Element_Kind (Decl) is
            when A_Statement =>  ------------------------------------------------- Statements
               declare
                  Labels : constant Defining_Name_List := Label_Names (Decl);
               begin
                  for I in Labels'Range loop
                     if Is_Equal (Name, Labels (I)) then
                        Check ((K_All, K_Label));
                        return;
                     end if;
                  end loop;
               end;

               -- It is not a label => must be a loop or block name
               case Statement_Kind (Decl) is
                  when A_Loop_Statement
                    | A_While_Loop_Statement
                    | A_For_Loop_Statement
                    =>
                     Check ((K_All, K_Stmt_Name, K_Loop_Name));
                  when A_Block_Statement
                    =>
                     Check ((K_All, K_Stmt_Name, K_Block_Name));
                  when others =>
                     Failure ("Unknown identifier from statement", Decl);
               end case;

            when A_Declaration =>  ----------------------------------------------- Declarations
               Decl_Kind := Declaration_Kind (Decl);

               if Decl_Kind in A_Full_Type_Declaration
                 and then Declaration_Kind (Corresponding_Type_Declaration (Decl))
                          in A_Private_Type_Declaration .. A_Private_Extension_Declaration
               then
                  -- This declaration is a full declaration of a private type.
                  -- It does not follow the rules for its own kind, but the ones for private
                  -- types (which are checked for the private declaration)
                  return;
               end if;

               case Decl_Kind is
                  when A_Renaming_Declaration =>
                     -- Get Decl and Decl_Kind from the renamed entity
                     case A_Renaming_Declaration (Decl_Kind) is
                        when An_Object_Renaming_Declaration =>

                           -- There are cases (like renaming of an indexed component) where
                           -- we want to go up a renaming, but Corrresponding_Base_Entity doesn't.
                           -- Hence the loop.
                           Going_Up_Renamings:
                             while Decl_Kind in A_Renaming_Declaration loop
                                Renamed := A4G_Bugs.Corresponding_Base_Entity (Decl);

                                loop
                                   case Expression_Kind (Renamed) is
                                      when A_Selected_Component =>
                                         Renamed := Selector (Renamed);
                                      when A_Slice
                                        | An_Indexed_Component
                                        =>
                                         Renamed := Prefix (Renamed);
                                      when A_Function_Call =>
                                         Decl_Kind := A_Constant_Declaration;
                                         exit Going_Up_Renamings;
                                      when An_Explicit_Dereference =>
                                         case Access_Type_Kind (Type_Declaration_View
                                                                (A4G_Bugs.Corresponding_Expression_Type
                                                                 (Prefix (Renamed))))
                                         is
                                            when A_Pool_Specific_Access_To_Variable
                                              | An_Access_To_Variable
                                              =>
                                               Decl_Kind := A_Variable_Declaration;
                                            when An_Access_To_Constant =>
                                               Decl_Kind := A_Constant_Declaration;
                                            when others =>
                                               Failure ("Unexpected type for dereference", Renamed);
                                         end case;
                                         exit Going_Up_Renamings;
                                      when A_Type_Conversion =>
                                         Renamed := Converted_Or_Qualified_Expression (Renamed);
                                      when An_Identifier
                                        | An_Enumeration_Literal
                                        | A_Character_Literal
                                        =>
                                         Decl      := Corresponding_Name_Declaration (Renamed);
                                         Decl_Kind := Declaration_Kind (Decl);
                                         exit;
                                      when others =>
                                         Failure ("Unexpected expression kind in renaming: "
                                                  & Expression_Kinds'Wide_Image (Expression_Kind (Renamed)),
                                                  Renamed);
                                   end case;
                                end loop;
                                Decl      := Corresponding_Name_Declaration (Renamed);
                                Decl_Kind := Declaration_Kind (Decl);
                             end loop Going_Up_Renamings;

                        when An_Exception_Renaming_Declaration =>
                           -- Decl not needed
                           Decl_Kind := An_Exception_Declaration;
                        when A_Package_Renaming_Declaration =>
                           -- Decl not needed
                           Decl_Kind := A_Package_Declaration;
                        when A_Procedure_Renaming_Declaration =>
                           -- Decl not needed
                           Decl_Kind := A_Procedure_Declaration;
                        when A_Function_Renaming_Declaration =>
                           -- Decl not needed
                           Decl_Kind := A_Function_Declaration;
                        when A_Generic_Package_Renaming_Declaration =>
                           -- Decl not needed
                           Decl_Kind := A_Generic_Package_Declaration;
                        when A_Generic_Procedure_Renaming_Declaration =>
                           -- Decl not needed
                           Decl_Kind := A_Generic_Procedure_Declaration;
                        when A_Generic_Function_Renaming_Declaration =>
                           -- Decl not needed
                           Decl_Kind := A_Generic_Function_Declaration;
                     end case;

                  when A_Subtype_Declaration =>
                     -- Unwind subtypes and derivations.
                     -- Do *not* unwind subtype if it designates a class-wide type,
                     -- otherwise, we have no way to tell later that it is class-wide
                     if not Is_Class_Wide_Subtype (Decl) then
                        -- Get Decl and Decl_Kind from the corresponding type
                        Decl      := Corresponding_First_Subtype (Decl);
                        Decl_Kind := Declaration_Kind (Decl);
                        Def       := Type_Declaration_View (Decl);
                        if Type_Kind (Def) in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition then
                           -- Subtype of a derived type
                           Decl      := Corresponding_Root_Type (Def);
                           Decl_Kind := Declaration_Kind (Decl);
                        end if;
                     end if;

                  when An_Incomplete_Type_Declaration =>
                     -- The name will be checked on the corresponding full type declaration
                     return;

                  when An_Ordinary_Type_Declaration =>
                     -- For derived types, get Decl and Decl_Kind from the corresponding type
                     Def := Type_Declaration_View (Decl);
                     if Type_Kind (Def) in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition then
                        Decl      := Corresponding_Root_Type (Def);
                        Decl_Kind := Declaration_Kind (Decl);
                     end if;

                  when others =>
                     null;
               end case;

               --
               -- Below is the place where the classification of declarations is made.
               -- It is the only place where something needs be changed if you want to
               -- change the hierarchy
               --

               case Decl_Kind is
                  when Not_A_Declaration =>
                     Failure ("Not a declaration", Decl);

                  when An_Ordinary_Type_Declaration =>  ------------------------ Types
                     Def := Type_Declaration_View (Decl);
                     case Type_Kind (Def) is
                        when An_Enumeration_Type_Definition =>
                           Check ((K_All, K_Type, K_Discrete_Type, K_Enumeration_Type));
                        when A_Signed_Integer_Type_Definition =>
                           Check ((K_All, K_Type, K_Discrete_Type, K_Integer_Type, K_Signed_Integer_Type));
                        when A_Modular_Type_Definition =>
                           Check ((K_All, K_Type, K_Discrete_Type, K_Integer_Type, K_Modular_Integer_Type));
                        when A_Floating_Point_Definition =>
                           Check ((K_All, K_Type, K_Discrete_Type, K_Floating_Point_Type));
                        when An_Ordinary_Fixed_Point_Definition =>
                           Check ((K_All, K_Type, K_Discrete_Type, K_Fixed_Point_Type, K_Binary_Fixed_Point_Type));
                        when A_Decimal_Fixed_Point_Definition =>
                           Check ((K_All, K_Type, K_Discrete_Type, K_Fixed_Point_Type, K_Decimal_Fixed_Point_Type));
                        when An_Unconstrained_Array_Definition
                          | A_Constrained_Array_Definition =>
                           Check ((K_All, K_Type, K_Array_Type));
                        when A_Record_Type_Definition =>
                           Check ((K_All, K_Type, K_Record_Type, K_Regular_Record_Type));
                        when A_Tagged_Record_Type_Definition =>
                           Check ((K_All, K_Type, K_Record_Type, K_Tagged_Type));
                        when An_Access_Type_Definition =>
                           if Access_Type_Kind (Def) in Access_To_Subprogram_Definition then
                              Check ((K_All, K_Type, K_Access_Type, K_Access_To_Sp_Type));
                           else
                              Accessed := Definitions.Subtype_Mark (Definitions.Access_To_Object_Definition (Def));
                              if A4G_Bugs.Attribute_Kind (Accessed) = A_Class_Attribute then
                                 -- Directly: type T is access T'Class
                                 Check ((K_All, K_Type, K_Access_Type, K_Access_To_Class_Type));
                              else
                                 -- Ignore a possible 'Base
                                 if A4G_Bugs.Attribute_Kind (Accessed) = A_Base_Attribute then
                                    Accessed := Prefix (Accessed);
                                 end if;

                                 -- Remove prefixes
                                 if Expression_Kind (Accessed) = A_Selected_Component then
                                    Accessed := Selector (Accessed);
                                 end if;

                                 -- Here, we should have a plain (sub)type identifier

                                 Accessed := Corresponding_Name_Declaration (Accessed);
                                 if Declaration_Kind (Accessed) = An_Incomplete_Type_Declaration then
                                    Accessed := Corresponding_Type_Declaration (Accessed);
                                 end if;

                                 if Declaration_Kind (Accessed) = A_Subtype_Declaration
                                   and then Is_Class_Wide_Subtype (Accessed)
                                 then
                                    -- Annoying special case: the access type designates a subtype that names
                                    -- a class-wide type. (i.e. subtype ST is T'Class; type Acc is access ST;)
                                    Check ((K_All, K_Type, K_Access_Type, K_Access_To_Class_Type));

                                 else
                                    -- Get rid of subtyping and derivations on the accessed type
                                    -- But we may have a mixture of formal or non-formal derivations...
                                    loop
                                       Accessed := Corresponding_First_Subtype (Accessed);
                                       Def      := Type_Declaration_View (Accessed);
                                       if Type_Kind (Def)
                                         in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
                                       then
                                          Accessed := Corresponding_Root_Type (Def);
                                       elsif Formal_Type_Kind (Def) = A_Formal_Derived_Type_Definition then
                                          Accessed := Corresponding_Name_Declaration (Definitions.Subtype_Mark (Def));
                                       else
                                          exit;
                                       end if;
                                    end loop;

                                    case Declaration_Kind (Accessed) is
                                       when An_Ordinary_Type_Declaration =>
                                          case Type_Kind (Type_Declaration_View (Accessed)) is
                                             when Not_A_Type_Definition =>
                                                Failure ("Unexpected accessed type 1", Accessed);
                                             when A_Tagged_Record_Type_definition
                                               | A_Derived_Record_Extension_Definition
                                               =>
                                                Check ((K_All, K_Type, K_Access_Type, K_Access_To_Tagged_Type));
                                             when others =>
                                                Check ((K_All, K_Type, K_Access_Type, K_Access_To_Regular_Type));
                                          end case;
                                       when A_Task_Type_Declaration =>
                                          Check ((K_All, K_Type, K_Access_Type, K_Access_To_Task_Type));
                                       when A_Protected_Type_Declaration =>
                                          Check ((K_All, K_Type, K_Access_Type, K_Access_To_Protected_Type));
                                       when A_Private_Type_Declaration =>
                                          Check ((K_All, K_Type, K_Access_Type, K_Access_To_Regular_Type));
                                       when A_Private_Extension_Declaration =>
                                          Check ((K_All, K_Type, K_Access_Type, K_Access_To_Tagged_Type));
                                       when A_Formal_Type_Declaration  =>
                                          case Formal_Type_Kind (Type_Declaration_View (Accessed)) is
                                             when Not_A_Formal_Type_Definition =>
                                                Failure ("not a formal type definition");
                                             when A_Formal_Derived_Type_Definition =>
                                                Failure ("Unexpected formal derived type", Accessed);
                                             when A_Formal_Discrete_Type_Definition
                                               | A_Formal_Signed_Integer_Type_Definition
                                               | A_Formal_Modular_Type_Definition
                                               | A_Formal_Floating_Point_Definition
                                               | A_Formal_Ordinary_Fixed_Point_Definition
                                               | A_Formal_Decimal_Fixed_Point_Definition
                                               | A_Formal_Access_Type_Definition
                                               | A_Formal_Private_Type_Definition
                                               | A_Formal_Unconstrained_Array_Definition
                                               | A_Formal_Constrained_Array_Definition
                                                =>
                                                Check ((K_All, K_Type, K_Access_Type, K_Access_To_Regular_Type));
                                             when A_Formal_Tagged_Private_Type_Definition =>
                                                Check ((K_All, K_Type, K_Access_Type, K_Access_To_Tagged_Type));
                                          end case;
                                       when others =>
                                          Failure ("Unexpected accessed type 2", Accessed);
                                    end case;
                                 end if;
                              end if;
                           end if;
                        when others =>
                           Failure ("Unexpected type kind: " & Type_Kinds'Wide_Image (Type_Kind (Def)));
                     end case;

                  when A_Subtype_Declaration =>
                     -- This should happen only if we didn't unwind subtyping because
                     -- the subtype designates a class-wide type (see above).
                     -- However, we recheck just to be sure...
                     Assert (Is_Class_Wide_Subtype (Decl), "Unexpected subtype declaration");
                     Check ((K_All, K_Type, K_Class_Type));

                  when An_Incomplete_Type_Declaration =>
                     Failure ("Unexpected incomplete declaration", Decl);

                  when A_Private_Type_Declaration =>
                     Check ((K_All, K_Type, K_Private_Type));

                  when A_Private_Extension_Declaration =>
                     Check ((K_All, K_Type, K_Private_Type, K_Private_Extension));

                  when A_Formal_Type_Declaration =>
                     Check ((K_All, K_Type, K_Generic_Formal_Type));

                  when An_Enumeration_Literal_Specification =>
                     Check ((K_All, K_Constant, K_Enumeration));

                  when A_Variable_Declaration =>  ------------------------ Constants, Variables, Parameters
                     Check ((K_All, K_Variable, K_Regular_Variable));

                  when A_Constant_Declaration
                    | A_Deferred_Constant_Declaration
                    =>
                     Check ((K_All, K_Constant, K_Regular_Constant));

                  when A_Choice_Parameter_Specification =>
                     Check ((K_All, K_Constant, K_Occurrence_Name));

                  when An_Entry_Index_Specification =>
                     Check ((K_All, K_Constant, K_Entry_Index));

                  when A_Loop_Parameter_Specification =>
                     Check ((K_All, K_Constant, K_Loop_Control));

                  when An_Integer_Number_Declaration =>
                     Check ((K_All, K_Constant, K_Named_Number, K_Integer_Number));

                  when A_Real_Number_Declaration =>
                     Check ((K_All, K_Constant, K_Named_Number, K_Real_Number));

                  when A_Parameter_Specification =>
                     case Mode_Kind (Decl) is
                        when A_Default_In_Mode
                          | An_In_Mode
                          =>
                           Check ((K_All, K_Constant, K_Sp_Formal_In));
                        when An_Out_Mode =>
                           Check ((K_All, K_Variable, K_Procedure_Formal_Out));
                        when An_In_Out_Mode =>
                           Check ((K_All, K_Variable, K_Procedure_Formal_In_Out));
                        when Not_A_Mode =>
                           Failure ("Unexpected mode: " & Mode_Kinds'Wide_Image (Mode_Kind (Decl)));
                     end case;

                  when A_Formal_Object_Declaration =>
                     case Mode_Kind (Decl) is
                        when A_Default_In_Mode
                          | An_In_Mode
                          =>
                           Check ((K_All, K_Constant, K_Generic_Formal_In));
                        when An_In_Out_Mode =>
                           Check ((K_All, K_Variable, K_Generic_Formal_In_Out));
                        when An_Out_Mode
                          | Not_A_Mode
                          =>
                           Failure ("Unexpected mode: " & Mode_Kinds'Wide_Image (Mode_Kind (Decl)));
                     end case;

                  when A_Discriminant_Specification =>
                     Check ((K_All, K_Variable, K_Field, K_Discriminant));

                  when A_Component_Declaration =>
                     -- We must determine whether it is declared within a record or protected type

                     -- Unwind variants
                     Def := Enclosing_Element (Decl);
                     while Definition_Kind (Def) = A_Variant loop
                        -- The variant is in a variant part in the record (or in another variant)
                        Def := Enclosing_Element (Enclosing_Element (Def));
                     end loop;

                     -- If the component is from a derived type, its declaration is inside
                     -- the type derivation => go up to the root type
                     if Type_Kind (Def)
                       in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
                     then
                        Def := Type_Declaration_View (Corresponding_Root_Type (Def));
                        if Definition_Kind (Def) = A_Type_Definition then
                           -- Must be a record type. Go to the record definition to match the
                           -- case of the underived type
                           Def := Definitions.Record_Definition (Def);
                        end if;
                     end if;

                     case Definition_Kind (Def) is
                        when A_Protected_Definition =>
                           Check ((K_All, K_Variable, K_Field, K_Protected_Field));
                        when A_Record_Definition =>
                           case Type_Kind (Enclosing_Element (Def)) is
                              when A_Record_Type_Definition
                                | A_Tagged_Record_Type_Definition
                                | A_Derived_Record_Extension_Definition
                                =>
                                 Check ((K_All, K_Variable, K_Field, K_Record_Field));
                              when others =>
                                 Failure ("Field not in record: "
                                          & Type_Kinds'Wide_Image (Type_Kind (Enclosing_Element (Def))), Def);
                           end case;
                        when others =>
                           Failure ("Not a record or protected field: "
                                    & Definition_Kinds'Wide_Image (Definition_Kind (Def)),
                                    Def);
                     end case;


                  when A_Procedure_Declaration  ------------------------ Subprograms
                    | A_Procedure_Instantiation
                    =>
                     -- To be honest, a procedure instantiation cannot be in a
                     -- protected specification
                     if Definition_Kind (Enclosing_Element (Decl)) = A_Protected_Definition then
                        Check ((K_All, K_Subprogram, K_Procedure, K_Protected_Procedure));
                     else
                        Check ((K_All, K_Subprogram, K_Procedure, K_Regular_Procedure));
                     end if;

                  when A_Formal_Procedure_Declaration =>
                     Check ((K_All, K_Subprogram, K_Procedure, K_Generic_Formal_Procedure));

                  when An_Entry_Declaration =>
                     case Definition_Kind (Enclosing_Element (Decl)) is
                        when A_Protected_Definition =>
                           Check ((K_All, K_Subprogram, K_Entry, K_Protected_Entry));
                        when A_Task_Definition =>
                           Check ((K_All, K_Subprogram, K_Entry, K_Task_Entry));
                        when others =>
                           Failure ("Entry not in task or protected", Enclosing_Element (Decl));
                     end case;

                  when A_Procedure_Body_Declaration
                    | A_Procedure_Body_Stub
                    =>
                     -- Check body only if there is no explicit spec
                     if Is_Nil (Corresponding_Declaration (Decl)) then
                        Check ((K_All, K_Subprogram, K_Procedure));
                     end if;

                  when A_Function_Declaration
                    | A_Function_Instantiation
                    =>
                     -- To be honest, a function instantiation cannot be in a
                     -- protected specification
                     if Definition_Kind (Enclosing_Element (Decl)) = A_Protected_Definition then
                        Check ((K_All, K_Subprogram, K_Function, K_Protected_Function));
                     else
                        Check ((K_All, K_Subprogram, K_Function, K_Regular_Function));
                     end if;

                  when A_Formal_Function_Declaration =>
                     Check ((K_All, K_Subprogram, K_Function, K_Generic_Formal_Function));

                  when A_Function_Body_Declaration
                    | A_Function_Body_Stub
                    =>
                     -- Check body only if there is no explicit spec
                     if Is_Nil (Corresponding_Declaration (Decl)) then
                        Check ((K_All, K_Subprogram, K_Function));
                     end if;

                  when A_Package_Declaration  ------------------------ Packages
                    | A_Package_Instantiation
                    =>
                     Check ((K_All, K_Package, K_Regular_Package));

                  when A_Formal_Package_Declaration
                    | A_Formal_Package_Declaration_With_Box
                    =>
                     Check ((K_All, K_Package, K_Generic_Formal_Package));

                  when A_Task_Type_Declaration =>  ------------------------ Tasks
                     Check ((K_All, K_Task, K_Task_Type));

                  when A_Single_Task_Declaration =>
                     Check ((K_All, K_Task, K_Task_Object));

                  when A_Protected_Type_Declaration =>  ------------------------ Protected
                     Check ((K_All, K_Protected, K_Protected_Type));

                  when A_Single_Protected_Declaration =>
                     Check ((K_All, K_Protected, K_Protected_Object));

                  when An_Exception_Declaration =>  ------------------------ Exceptions
                     Check ((K_All, K_Exception));

                  when A_Generic_Procedure_Declaration =>  ------------------------ Generics
                     Check ((K_All, K_Generic, K_Generic_Sp, K_Generic_Procedure));

                  when A_Generic_Function_Declaration =>
                     Check ((K_All, K_Generic, K_Generic_Sp, K_Generic_Function));

                  when A_Generic_Package_Declaration =>
                     Check ((K_All, K_Generic, K_Generic_Package));

                  when A_Package_Body_Declaration  ------------------------ Not processed
                    | A_Package_Body_Stub
                    | A_Task_Body_Declaration
                    | A_Task_Body_Stub
                    | A_Protected_Body_Declaration
                    | A_Protected_Body_Stub
                    | An_Entry_Body_Declaration
                    =>
                     -- We do not check bodies of units that require a specification
                     null;

                  when A_Renaming_Declaration =>
                     Failure ("Unexpected renaming", Decl);
               end case;

            when others =>
               Failure ("Not a statement or declaration", Decl);
         end case;
      end;
   end Process_Defining_Name;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access);
end Rules.Naming_Convention;
