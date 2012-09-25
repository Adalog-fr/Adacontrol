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
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Unchecked_Deallocation;

-- ASIS
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  String_Matching,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Scope_Manager,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);

package body Rules.Naming_Convention is
   use Framework, Framework.Control_Manager, Framework.Variables.Shared_Types;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   -- Rule variables
   Default_Case_Sensitivity : aliased Switch_Type.Object := (Value => Off);

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
                          K_Interface_Type,
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
                          K_Generic_Function,
                    K_Renaming,
                       K_Object_Renaming,
                       K_Exception_Renaming,
                       K_Package_Renaming,
                       K_Subprogram_Renaming,
                          K_Procedure_Renaming,
                          K_Function_Renaming,
                       K_Generic_Renaming,
                          K_Generic_Package_Renaming,
                          K_Generic_Sp_Renaming,
                             K_Generic_Procedure_Renaming,
                             K_Generic_Function_Renaming
                );
   Max_Hierarchy_Depth : constant := 5;
   -- Maximum logical depth of the above hierarchy

   package Keys_Flags_Utilities is new Framework.Language.Flag_Utilities (Keys, "K_");
   use Keys_Flags_Utilities;

   type Key_Set_Index is range 1 .. Max_Hierarchy_Depth;
   type Key_Set is array (Key_Set_Index range <>) of Keys;

   type Visibility is (Scope_Global, Scope_Local, Scope_Unit);
   package Visibility_Utilities is new Framework.Language.Modifier_Utilities (Modifiers  => Visibility,
                                                                              Prefix     => "SCOPE_");
   subtype Scope_Set is Visibility_Utilities.Modifier_Set;

   type Usage_Rec;
   type Usage_Rec_Access is access Usage_Rec;
   type Pattern_Access is access String_Matching.Compiled_Pattern;
   type Usage_Rec is new Basic_Rule_Context with
      record
         Scopes    : Scope_Set;
         Is_Others : Boolean := False;
         Pattern   : Pattern_Access;
         Next      : Usage_Rec_Access;
      end record;

   type Rule_Rec is
      record
         Is_Root        : Boolean := False;
         First_Positive : Usage_Rec_Access;
         First_Negative : Usage_Rec_Access;
      end record;
   Usage : array (Keys) of Rule_Rec;

   -----------
   -- Clear --
   -----------

   procedure Clear (Rec : in out Usage_Rec_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Usage_Rec, Usage_Rec_Access);
      procedure Free is new Ada.Unchecked_Deallocation (String_Matching.Compiled_Pattern, Pattern_Access);
      Temp : Usage_Rec_Access;
   begin
      while Rec /= null loop
         Temp := Rec.Next;
         Free (Rec.Pattern);
         Free (Rec);
         Rec := Temp;
      end loop;
   end Clear;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Variables, Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control the form of allowed (or forbidden) names in declarations");
      User_Message;
      User_Message  ("Parameter 1: [root] [others] [global|local|unit]");
      Help_On_Flags ("                ");
      User_Message  ("Parameter 2..N: [case_sensitive|case_insensitive] [not] ""<name pattern>""");
      User_Message ("Variables:");
      Help_On_Variable (Rule_Id & ".Default_Case_Sensitivity");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Characters.Handling, Ada.Exceptions, Framework.Language, String_Matching;
      use Visibility_Utilities;

      Key       : Keys;
      Scopes    : Scope_Set;
      Is_Root   : Boolean;
      Is_Others : Boolean;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "kind of filter required");
      end if;
      Is_Root   := Get_Modifier ("ROOT");
      Is_Others := Get_Modifier ("OTHERS");
      Scopes    := Visibility_Utilities.Get_Modifier_Set;
      if Scopes = Empty_Set then
         Scopes := Full_Set;
      end if;
      Key := Get_Flag_Parameter (Allow_Any => False);

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one pattern required");
      end if;

      while Parameter_Exists loop
         declare
            Ignore_Case : constant Boolean     := Get_Modifier (True_KW  => "CASE_INSENSITIVE",
                                                                False_KW => "CASE_SENSITIVE",
                                                                Default  => Default_Case_Sensitivity.Value = Off);
            Is_Not      : constant Boolean     := Get_Modifier ("NOT");
            Pattern     : constant Wide_String := Get_String_Parameter;
         begin
            if Is_Not then
               Usage (Key) := (Is_Root => Usage (Key).Is_Root or Is_Root,
                               First_Positive => Usage (Key).First_Positive,
                               First_Negative => new Usage_Rec'
                                                    (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                                     Scopes    => Scopes,
                                                     Is_Others => Is_Others,
                                                     Pattern   => new Compiled_Pattern'(Compile (Pattern, Ignore_Case)),
                                                     Next      => Usage (Key).First_Negative)
                              );
            else
               Usage (Key) := (Is_Root => Usage (Key).Is_Root or Is_Root,
                               First_Positive => new Usage_Rec'
                                                    (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                                                     Scopes    => Scopes,
                                                     Is_Others => Is_Others,
                                                     Pattern   => new Compiled_Pattern'(Compile (Pattern, Ignore_Case)),
                                                     Next      => Usage (Key).First_Positive),
                               First_Negative => Usage (Key).First_Negative
                              );
            end if;
         exception
            when Occur: Pattern_Error =>
               Parameter_Error (Rule_Id,
                                "Incorrect pattern: " & Pattern
                                & " (" & To_Wide_String (Exception_Message (Occur)) & ')');
         end;
      end loop;

      Rule_Used := True;
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
            for I in Usage'Range loop
               Clear (Usage (I).First_Positive);
               Clear (Usage (I).First_Negative);
               Usage (I).Is_Root := False;
            end loop;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Is_Used --
   -------------

   function Is_Used (Filters : Key_Set) return Boolean is
   begin
      for I in Filters'Range loop
         if Usage (Filters (I)).First_Positive /= null
           or Usage (Filters (I)).First_Negative /= null
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Used;

   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Name : in Asis.Defining_Name) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Utilities;

      Decl : Asis.Declaration;
      -- The (true) enclosing declaration of Name

      -- Applicable rules must be given in order of decreasing generality
      procedure Check (Name_Str : in Wide_String; Set : in Key_Set) is
         use Framework.Scope_Manager;

         Is_Program_Unit        : constant Boolean := Is_Equal (Decl, Current_Scope);
         Is_Compilation_Unit    : constant Boolean := Is_Program_Unit and Current_Depth = 1;
         Is_Global              : Boolean;
         Scopes_Mask            : Scope_Set;
         Positive_Pattern_Found : Boolean := False;

         procedure Check_One (Key : in Keys) is
            use String_Matching, Visibility_Utilities, Framework.Reports;

            Current                 : Usage_Rec_Access;
            Matches                 : Boolean;
            Last_Applicable_Pattern : Usage_Rec_Access := null;
         begin
            Current := Usage (Key).First_Negative;
            while Current /= null loop
               if (Current.Scopes and Scopes_Mask) /= Empty_Set
                 and not (Current.Is_Others and Positive_Pattern_Found)
               then
                  Matches := Match (Name_Str, Current.Pattern.all);
                  if Matches then
                     Report (Rule_Id,
                             Current.all,
                             Get_Location (Name),
                             "Name does not follow naming rule for """
                             & Image (Current.Scopes, Default => Full_Set) & Image (Key, Lower_Case)
                             & """: """
                             & Defining_Name_Image (Name) & '"');
                  end if;
               end if;
               Current  := Current.Next;
            end loop;

            Current := Usage (Key).First_Positive;
            while Current /= null loop
               if (Current.Scopes and Scopes_Mask) /= Empty_Set
                 and not (Current.Is_Others and Positive_Pattern_Found)
               then
                  Last_Applicable_Pattern := Current;
                  Matches := Match (Name_Str, Current.Pattern.all);
                  if Matches then
                     Positive_Pattern_Found := True;
                     return;
                  end if;
               end if;
               exit when Current.Next = null;
               Current := Current.Next;
            end loop;

            -- No match found here => error, unless there were no positive patterns, or
            --                        all positive patterns were "others"
            -- Note that Last_Applicable_Pattern points to the last non-others pattern checked, which is
            -- the first one given by the user since we chain on head.
            if Last_Applicable_Pattern /= null then
               Positive_Pattern_Found := True;
               Report (Rule_Id,
                       Last_Applicable_Pattern.all,
                       Get_Location (Name),
                       "Name does not follow naming rule for """
                       & Image (Last_Applicable_Pattern.Scopes, Default => Full_Set) & Image (Key, Lower_Case)
                       & """: """
                       & Defining_Name_Image (Name) & '"');
            end if;
         end Check_One;

      begin  -- Check
         if Is_Compilation_Unit then
            Is_Global := False;
         elsif Is_Program_Unit then
            Is_Global := Is_Enclosing_Scope_Global;
         else
            Is_Global := Is_Current_Scope_Global;
         end if;
         Scopes_Mask := (Scope_Local  => not Is_Global and not Is_Compilation_Unit,
                         Scope_Global => Is_Global,
                         Scope_Unit   => Is_Compilation_Unit);

         for I in reverse Set'Range loop
            Check_One (Set (I));
            exit when Usage (Set (I)).Is_Root;
         end loop;
      end Check;

      use Asis.Definitions;
   begin    -- Process_Defining_Name
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Name_Str  : constant Wide_String := Defining_Name_Image (Name);
         Renamed   : Asis.Element;
         Renamed_T : Asis.Element;
         Decl_Kind : Asis.Declaration_Kinds;
         Def       : Asis.Definition;
         Accessed  : Asis.Element;

      begin
         Decl := Enclosing_Element (Name);
         while Defining_Name_Kind (Decl) = A_Defining_Expanded_Name loop
            -- Name was the name of a child compilation unit
            Decl := Enclosing_Element (Decl);
         end loop;

         -- Every path in the following case statement must end with a call to Check,
         -- and perform nothing after.
         -- There is nothing after the case statement.
         -- It is therefore irrelevant whether the call to Check is followed by a return or not.
         -- In most cases, there is no return, however in some cases there is a return when the
         -- call to check is deep in the statements, to simplify the structure.
         case Element_Kind (Decl) is
            when A_Statement =>  ------------------------------------------------- Statements
               declare
                  Labels : constant Defining_Name_List := Label_Names (Decl);
               begin
                  for I in Labels'Range loop
                     if Is_Equal (Name, Labels (I)) then
                        Check (Name_Str, (K_All, K_Label));
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
                     Check (Name_Str, (K_All, K_Stmt_Name, K_Loop_Name));
                  when A_Block_Statement
                    =>
                     Check (Name_Str, (K_All, K_Stmt_Name, K_Block_Name));
                  when others =>
                     Failure ("Unknown identifier from statement", Decl);
               end case;

            when A_Declaration =>  ----------------------------------------------- Declarations
               Decl_Kind := Declaration_Kind (Decl);

               begin
                  if Decl_Kind in A_Full_Type_Declaration
                    and then Declaration_Kind (Corresponding_Type_Declaration (Decl))
                              in A_Private_Type_Declaration .. A_Private_Extension_Declaration
                  then
                     -- This declaration is a full declaration of a private type.
                     -- It does not follow the rules for its own kind, but the ones for private
                     -- types (which are checked for the private declaration)
                     return;
                  end if;
               exception
                  when Asis.Exceptions.ASIS_Failed =>
                     -- A4G BUG in Gnat/GPL, Gnat/GAP, fixed in 5.04 and above
                     -- Corresponding_Type_Declaration fails for types declared in child units
                     -- and separate units.
                     -- Apparently, this does not happen for full declarations of private types,
                     -- therefore we can ignore the problem
                     A4G_Bugs.Trace_Bug ("Rules.Naming_Convention.Process_Defining_Name: ASIS_Failed");
               end;

               case Decl_Kind is
                  when A_Renaming_Declaration =>
                     -- Get Decl and Decl_Kind from the renamed entity
                     case A_Renaming_Declaration (Decl_Kind) is
                        when An_Object_Renaming_Declaration =>
                           if not Is_Used ((K_Renaming, K_Object_Renaming)) then
                              -- There are cases (like renaming of an indexed component) where
                              -- we want to go up a renaming, but Corrresponding_Base_Entity doesn't.
                              -- Hence the loop.
                              -- We can't use Ultimate_Name, because we need a different treatment of dereferences
                              Going_Up_Renamings :
                              while Decl_Kind in A_Renaming_Declaration loop
                                 Renamed := Renamed_Entity (Decl);

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
                                       when A_Type_Conversion =>
                                          Renamed := Converted_Or_Qualified_Expression (Renamed);
                                       when An_Identifier
                                          | An_Enumeration_Literal
                                          | A_Character_Literal
                                            =>
                                          exit;
                                       when An_Explicit_Dereference =>
                                          Renamed_T := Thick_Queries.Corresponding_Expression_Type_Definition
                                                        (Prefix (Renamed));
                                          if Definition_Kind (Renamed_T) = An_Access_Definition then
                                             -- anonymous access type
                                             -- 2005 can occur in many declarations...
                                             Decl := Enclosing_Element (Renamed);
                                             case Access_Definition_Kind (Renamed_T) is
                                                when Not_An_Access_Definition =>
                                                   Failure ("Process_Defining_Name: Not an access definition",
                                                            Renamed);
                                                when An_Anonymous_Access_To_Variable =>
                                                   Decl_Kind := A_Variable_Declaration;
                                                when An_Anonymous_Access_To_Constant =>
                                                   Decl_Kind := A_Constant_Declaration;
                                                when others =>
                                                   Failure ("Process_Defining_Name: Unexpected anonymous dereference",
                                                            Renamed);
                                             end case;
                                          else
                                             -- regular access type
                                             case Access_Type_Kind (Renamed_T) is
                                                when A_Pool_Specific_Access_To_Variable
                                                   | An_Access_To_Variable
                                                   =>
                                                   Decl_Kind := A_Variable_Declaration;
                                                when An_Access_To_Constant =>
                                                   Decl_Kind := A_Constant_Declaration;
                                                when others =>
                                                   Failure ("Process_Defining_Name: Unexpected named dereference",
                                                            Renamed);
                                             end case;
                                          end if;
                                          exit Going_Up_Renamings;

                                       when others =>
                                          Failure ("Process_Defining_Name: Unexpected expression kind in renaming: "
                                                   & Expression_Kinds'Wide_Image (Expression_Kind (Renamed)),
                                                   Renamed);
                                    end case;
                                 end loop;
                                 Decl      := Corresponding_Name_Declaration (Renamed);
                                 Decl_Kind := Declaration_Kind (Decl);
                              end loop Going_Up_Renamings;
                           end if;
                        when An_Exception_Renaming_Declaration =>
                           if not Is_Used ((K_Renaming, K_Exception_Renaming)) then
                              -- Decl not needed
                              Decl_Kind := An_Exception_Declaration;
                           end if;
                        when A_Package_Renaming_Declaration =>
                           if not Is_Used ((K_Renaming, K_Package_Renaming)) then
                              -- Decl not needed
                              Decl_Kind := A_Package_Declaration;
                           end if;
                        when A_Procedure_Renaming_Declaration =>
                           if not Is_Used ((K_Renaming, K_Subprogram_Renaming, K_Procedure_Renaming)) then
                              if not Is_Equal (Decl, Corresponding_Declaration (Decl)) then
                                 -- Renaming as body
                                 return;
                              end if;
                              -- Decl not needed
                              Decl_Kind := A_Procedure_Declaration;
                           end if;
                        when A_Function_Renaming_Declaration =>
                           if not Is_Used ((K_Renaming, K_Subprogram_Renaming, K_Function_Renaming)) then
                              if not Is_Equal (Decl, Corresponding_Declaration (Decl)) then
                                 -- Renaming as body
                                 return;
                              end if;
                              -- Decl not needed
                              Decl_Kind := A_Function_Declaration;
                           end if;
                        when A_Generic_Package_Renaming_Declaration =>
                           if not Is_Used ((K_Renaming, K_Generic_Renaming, K_Generic_Package_Renaming)) then
                              -- Decl not needed
                              Decl_Kind := A_Generic_Package_Declaration;
                           end if;
                        when A_Generic_Procedure_Renaming_Declaration =>
                           if not Is_Used ((K_Renaming,            K_Generic_Renaming,
                                            K_Generic_Sp_Renaming, K_Generic_Procedure_Renaming))
                           then
                              -- Decl not needed
                              Decl_Kind := A_Generic_Procedure_Declaration;
                           end if;
                        when A_Generic_Function_Renaming_Declaration =>
                           if not Is_Used ((K_Renaming,            K_Generic_Renaming,
                                            K_Generic_Sp_Renaming, K_Generic_Function_Renaming))
                           then
                              -- Decl not needed
                              Decl_Kind := A_Generic_Function_Declaration;
                           end if;
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
                           Check (Name_Str, (K_All, K_Type, K_Discrete_Type, K_Enumeration_Type));
                        when A_Signed_Integer_Type_Definition =>
                           Check (Name_Str, (K_All, K_Type, K_Discrete_Type, K_Integer_Type, K_Signed_Integer_Type));
                        when A_Modular_Type_Definition =>
                           Check (Name_Str, (K_All, K_Type, K_Discrete_Type, K_Integer_Type, K_Modular_Integer_Type));
                        when A_Floating_Point_Definition =>
                           Check (Name_Str, (K_All, K_Type, K_Discrete_Type, K_Floating_Point_Type));
                        when An_Ordinary_Fixed_Point_Definition =>
                           Check (Name_Str,
                                  (K_All, K_Type, K_Discrete_Type, K_Fixed_Point_Type, K_Binary_Fixed_Point_Type));
                        when A_Decimal_Fixed_Point_Definition =>
                           Check (Name_Str,
                                  (K_All, K_Type, K_Discrete_Type, K_Fixed_Point_Type, K_Decimal_Fixed_Point_Type));
                        when An_Unconstrained_Array_Definition
                          | A_Constrained_Array_Definition =>
                           Check (Name_Str, (K_All, K_Type, K_Array_Type));
                        when A_Record_Type_Definition =>
                           Check (Name_Str, (K_All, K_Type, K_Record_Type, K_Regular_Record_Type));
                        when A_Tagged_Record_Type_Definition =>
                           Check (Name_Str, (K_All, K_Type, K_Record_Type, K_Tagged_Type));
                        when An_Interface_Type_Definition =>
                           Check (Name_Str, (K_All, K_Type, K_Record_Type, K_Interface_Type));
                        when An_Access_Type_Definition =>
                           if Access_Type_Kind (Def) in Access_To_Subprogram_Definition then
                              Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_SP_Type));
                              return;
                           end if;

                           -- Here, we have an acces to object
                           Accessed := Subtype_Simple_Name (Definitions.Access_To_Object_Definition (Def));
                           if Attribute_Kind (Accessed) = A_Class_Attribute then
                              -- Directly: type T is access T'Class
                              Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Class_Type));
                              return;
                           end if;

                           -- Ignore a possible 'Base
                           if Attribute_Kind (Accessed) = A_Base_Attribute then
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
                              if Is_Nil (Accessed) then
                                 -- The full declaration of the accessed type is not in the context.
                                 -- We cannot know the real nature of the accessed type.
                                 -- Limit the check to Access_Type, and hope the user will rerun AdaControl
                                 -- on the full program.
                                 Check (Name_Str, (K_All, K_Type, K_Access_Type));
                                 return;
                              end if;
                           end if;

                           if Declaration_Kind (Accessed) = A_Subtype_Declaration
                             and then Is_Class_Wide_Subtype (Accessed)
                           then
                              -- Annoying special case: the access type designates a subtype that names
                              -- a class-wide type. (i.e. subtype ST is T'Class; type Acc is access ST;)
                              Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Class_Type));
                              return;
                           end if;

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
                                 Accessed := Corresponding_Name_Declaration (Subtype_Simple_Name (Def));
                              else
                                 exit;
                              end if;
                           end loop;

                           case Declaration_Kind (Accessed) is
                              when An_Ordinary_Type_Declaration =>
                                 case Type_Kind (Type_Declaration_View (Accessed)) is
                                    when Not_A_Type_Definition =>
                                       Failure ("Unexpected accessed type 1", Accessed);
                                    when A_Tagged_Record_Type_Definition
                                      | A_Derived_Record_Extension_Definition
                                      =>
                                       Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Tagged_Type));
                                    when others =>
                                       Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Regular_Type));
                                 end case;
                              when A_Task_Type_Declaration =>
                                 Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Task_Type));
                              when A_Protected_Type_Declaration =>
                                 Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Protected_Type));
                              when A_Private_Type_Declaration =>
                                 Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Regular_Type));
                              when A_Private_Extension_Declaration =>
                                 Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Tagged_Type));
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
                                       Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Regular_Type));
                                    when A_Formal_Tagged_Private_Type_Definition =>
                                       Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Tagged_Type));
                                    when others => -- Compatibility Ada 2005
                                       null;
                                 end case;
                              when others =>
                                 Failure ("Unexpected accessed type 2", Accessed);
                           end case;

                        when others =>
                           Failure ("Unexpected type kind: " & Type_Kinds'Wide_Image (Type_Kind (Def)));
                     end case;

                  when A_Subtype_Declaration =>
                     -- This should happen only if we didn't unwind subtyping because
                     -- the subtype designates a class-wide type (see above).
                     -- However, we recheck just to be sure...
                     Assert (Is_Class_Wide_Subtype (Decl), "Unexpected subtype declaration");
                     Check (Name_Str, (K_All, K_Type, K_Class_Type));

                  when An_Incomplete_Type_Declaration =>
                     Failure ("Unexpected incomplete declaration", Decl);

                  when A_Private_Type_Declaration =>
                     Check (Name_Str, (K_All, K_Type, K_Private_Type));

                  when A_Private_Extension_Declaration =>
                     Check (Name_Str, (K_All, K_Type, K_Private_Type, K_Private_Extension));

                  when A_Formal_Type_Declaration =>
                     Check (Name_Str, (K_All, K_Type, K_Generic_Formal_Type));

                  when An_Enumeration_Literal_Specification =>
                     Check (Name_Str, (K_All, K_Constant, K_Enumeration));

                  when A_Variable_Declaration =>  ------------------------ Constants, Variables, Parameters
                     Check (Name_Str, (K_All, K_Variable, K_Regular_Variable));

                  when A_Constant_Declaration
                    | A_Deferred_Constant_Declaration
                    =>
                     Check (Name_Str, (K_All, K_Constant, K_Regular_Constant));

                  when A_Choice_Parameter_Specification =>
                     Check (Name_Str, (K_All, K_Constant, K_Occurrence_Name));

                  when An_Entry_Index_Specification =>
                     Check (Name_Str, (K_All, K_Constant, K_Entry_Index));

                  when A_Loop_Parameter_Specification =>
                     Check (Name_Str, (K_All, K_Constant, K_Loop_Control));

                  when An_Integer_Number_Declaration =>
                     Check (Name_Str, (K_All, K_Constant, K_Named_Number, K_Integer_Number));

                  when A_Real_Number_Declaration =>
                     Check (Name_Str, (K_All, K_Constant, K_Named_Number, K_Real_Number));

                  when A_Parameter_Specification =>
                     -- Check if it is the "real" declaration of the parameter
                     declare
                        Sp_Decl : constant Asis.Declaration := Enclosing_Element (Decl);
                     begin
                        case Declaration_Kind (Sp_Decl) is
                           when A_Procedure_Declaration
                              | A_Generic_Procedure_Declaration
                              | A_Formal_Procedure_Declaration
                              | A_Function_Declaration
                              | A_Generic_Function_Declaration
                              | A_Formal_Function_Declaration
                              | An_Entry_Declaration
                              =>
                              -- Specifications: always OK
                              null;
                           when A_Procedure_Body_Declaration
                              | A_Procedure_Body_Stub
                              | A_Function_Body_Declaration
                              | A_Function_Body_Stub
                              =>
                              -- Bodies: checked only if there is no other explicit declaration
                              if Is_Subunit (Sp_Decl) then
                                 -- There is at least a stub
                                 return;
                              end if;
                              if not Is_Nil (Corresponding_Declaration (Sp_Decl)) then
                                 -- There is an explicit specification
                                 return;
                              end if;
                           when An_Entry_Body_Declaration =>
                              -- Those always have a spec
                              return;
                           when A_Procedure_Renaming_Declaration
                              | A_Function_Renaming_Declaration
                              =>
                              -- Don't check if it is a renaming-as-body
                              if not Is_Equal (Sp_Decl, Corresponding_Declaration (Sp_Decl)) then
                                 -- Renaming-as-body
                                 return;
                              end if;
                           when Not_A_Declaration =>
                              -- Some weird cases...
                              -- Can be an accept statement, but there is always a corresponding entry declaration
                              case Statement_Kind (Sp_Decl) is
                                 when An_Accept_Statement =>
                                    return;
                                 when others =>
                                    null;
                              end case;

                              -- Can be An_Access_Definition (or A_Type_Definition) if inside an access to subprogram
                              --   => keep it
                              case Definition_Kind (Sp_Decl) is
                                 when A_Type_Definition
                                    | A_Formal_Type_Definition
                                    | An_Access_Definition =>
                                    null;
                                 when others =>
                                    Failure ("Parameter specification not in callable statement (1)", Sp_Decl);
                              end case;
                           when others =>
                              Failure ("Parameter specification not in callable statement (2)", Sp_Decl);
                        end case;
                     end;

                     case Mode_Kind (Decl) is
                        when A_Default_In_Mode
                          | An_In_Mode
                          =>
                           Check (Name_Str, (K_All, K_Constant, K_Sp_Formal_In));
                        when An_Out_Mode =>
                           Check (Name_Str, (K_All, K_Variable, K_Procedure_Formal_Out));
                        when An_In_Out_Mode =>
                           Check (Name_Str, (K_All, K_Variable, K_Procedure_Formal_In_Out));
                        when Not_A_Mode =>
                           Failure ("Unexpected mode: " & Mode_Kinds'Wide_Image (Mode_Kind (Decl)));
                     end case;

                  when A_Formal_Object_Declaration =>
                     case Mode_Kind (Decl) is
                        when A_Default_In_Mode
                          | An_In_Mode
                          =>
                           Check (Name_Str, (K_All, K_Constant, K_Generic_Formal_In));
                        when An_In_Out_Mode =>
                           Check (Name_Str, (K_All, K_Variable, K_Generic_Formal_In_Out));
                        when An_Out_Mode
                          | Not_A_Mode
                          =>
                           Failure ("Unexpected mode: " & Mode_Kinds'Wide_Image (Mode_Kind (Decl)));
                     end case;

                  when A_Discriminant_Specification =>
                     Check (Name_Str, (K_All, K_Variable, K_Field, K_Discriminant));

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
                           Check (Name_Str, (K_All, K_Variable, K_Field, K_Protected_Field));
                        when A_Record_Definition
                           | A_Private_Extension_Definition
                           =>
                           Check (Name_Str, (K_All, K_Variable, K_Field, K_Record_Field));
                        when others =>
                           Failure ("Not a record or protected field: "
                                    & Definition_Kinds'Wide_Image (Definition_Kind (Def)),
                                    Def);
                     end case;


                  when A_Procedure_Declaration  ------------------------ Subprograms
                     | A_Null_Procedure_Declaration
                     | A_Procedure_Instantiation
                     =>
                     -- To be honest, a procedure instantiation cannot be in a
                     -- protected specification - neither a null procedure. Oh, well..
                     if Definition_Kind (Enclosing_Element (Decl)) = A_Protected_Definition then
                        Check (Name_Str, (K_All, K_Subprogram, K_Procedure, K_Protected_Procedure));
                     else
                        Check (Name_Str, (K_All, K_Subprogram, K_Procedure, K_Regular_Procedure));
                     end if;

                  when A_Formal_Procedure_Declaration =>
                     Check (Name_Str, (K_All, K_Subprogram, K_Procedure, K_Generic_Formal_Procedure));

                  when An_Entry_Declaration =>
                     case Definition_Kind (Enclosing_Element (Decl)) is
                        when A_Protected_Definition =>
                           Check (Name_Str, (K_All, K_Subprogram, K_Entry, K_Protected_Entry));
                        when A_Task_Definition =>
                           Check (Name_Str, (K_All, K_Subprogram, K_Entry, K_Task_Entry));
                        when others =>
                           Failure ("Entry not in task or protected", Enclosing_Element (Decl));
                     end case;

                  when A_Procedure_Body_Declaration
                    | A_Procedure_Body_Stub
                    =>
                     -- Check body only if there is no explicit spec
                     if Is_Nil (Corresponding_Declaration (Decl)) then
                        Check (Name_Str, (K_All, K_Subprogram, K_Procedure));
                     end if;

                  when A_Function_Declaration
                    | A_Function_Instantiation
                    =>
                     -- To be honest, a function instantiation cannot be in a
                     -- protected specification
                     if Definition_Kind (Enclosing_Element (Decl)) = A_Protected_Definition then
                        Check (Name_Str, (K_All, K_Subprogram, K_Function, K_Protected_Function));
                     else
                        Check (Name_Str, (K_All, K_Subprogram, K_Function, K_Regular_Function));
                     end if;

                  when A_Formal_Function_Declaration =>
                     Check (Name_Str, (K_All, K_Subprogram, K_Function, K_Generic_Formal_Function));

                  when A_Function_Body_Declaration
                    | A_Function_Body_Stub
                    =>
                     -- Check body only if there is no explicit spec
                     if Is_Nil (Corresponding_Declaration (Decl)) then
                        Check (Name_Str, (K_All, K_Subprogram, K_Function));
                     end if;

                  when A_Package_Declaration  ------------------------ Packages
                    | A_Package_Instantiation
                    =>
                     Check (Name_Str, (K_All, K_Package, K_Regular_Package));

                  when A_Formal_Package_Declaration
                    | A_Formal_Package_Declaration_With_Box
                    =>
                     Check (Name_Str, (K_All, K_Package, K_Generic_Formal_Package));

                  when A_Task_Type_Declaration =>  ------------------------ Tasks
                     Check (Name_Str, (K_All, K_Task, K_Task_Type));

                  when A_Single_Task_Declaration =>
                     Check (Name_Str, (K_All, K_Task, K_Task_Object));

                  when A_Protected_Type_Declaration =>  ------------------------ Protected
                     Check (Name_Str, (K_All, K_Protected, K_Protected_Type));

                  when A_Single_Protected_Declaration =>
                     Check (Name_Str, (K_All, K_Protected, K_Protected_Object));

                  when An_Exception_Declaration =>  ------------------------ Exceptions
                     Check (Name_Str, (K_All, K_Exception));

                  when A_Generic_Procedure_Declaration =>  ------------------------ Generics
                     Check (Name_Str, (K_All, K_Generic, K_Generic_Sp, K_Generic_Procedure));

                  when A_Generic_Function_Declaration =>
                     Check (Name_Str, (K_All, K_Generic, K_Generic_Sp, K_Generic_Function));

                  when A_Generic_Package_Declaration =>
                     Check (Name_Str, (K_All, K_Generic, K_Generic_Package));

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
                     case A_Renaming_Declaration (Decl_Kind) is
                        when An_Object_Renaming_Declaration =>
                           Check (Name_Str, (K_Renaming, K_Object_Renaming));
                        when An_Exception_Renaming_Declaration =>
                           Check (Name_Str, (K_Renaming, K_Exception_Renaming));
                        when A_Package_Renaming_Declaration =>
                           Check (Name_Str, (K_Renaming, K_Package_Renaming));
                        when A_Procedure_Renaming_Declaration =>
                           Check (Name_Str, (K_Renaming, K_Subprogram_Renaming, K_Procedure_Renaming));
                        when A_Function_Renaming_Declaration =>
                           Check (Name_Str, (K_Renaming, K_Subprogram_Renaming, K_Function_Renaming));
                        when A_Generic_Package_Renaming_Declaration =>
                           Check (Name_Str, (K_Renaming, K_Generic_Renaming, K_Generic_Package_Renaming));
                        when A_Generic_Procedure_Renaming_Declaration =>
                           Check (Name_Str, (K_Renaming,            K_Generic_Renaming,
                                             K_Generic_Sp_Renaming, K_Generic_Procedure_Renaming));
                        when A_Generic_Function_Renaming_Declaration =>
                           Check (Name_Str, (K_Renaming,            K_Generic_Renaming,
                                             K_Generic_Sp_Renaming, K_Generic_Function_Renaming));
                     end case;
                  when others =>   -- Compatibility Ada 2005
                     null;
               end case;

            when others =>
               Failure ("Not a statement or declaration", Decl);
         end case;
      end;
   end Process_Defining_Name;

begin  -- Rules.Naming_Convention
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
   Framework.Variables.Register (Default_Case_Sensitivity'Access,
                                 Variable_Name => Rule_Id & ".DEFAULT_CASE_SENSITIVITY");
end Rules.Naming_Convention;
