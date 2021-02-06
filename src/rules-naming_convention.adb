----------------------------------------------------------------------
--  Rules.Naming_Convention - Package body                          --
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
  Ada.Characters.Handling,
  Ada.Characters.Latin_1,
  Ada.Exceptions,
  Ada.Wide_Characters.Handling,
  Ada.Wide_Text_IO;

-- ASIS
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Limited_Views,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Implementation_Options,
  Scope_Manager,
  String_Matching,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Pattern_Queues,
  Framework.Pattern_Queues_Matchers,
  Framework.Reports.Fixes,
  Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language);
pragma Elaborate (Framework.Language.Shared_Keys);

package body Rules.Naming_Convention is
   use Framework, Framework.Control_Manager, Framework.Variables.Shared_Types, Framework.Language.Shared_Keys;

   -- Algorithm:
   --
   -- The classification of elements is done by a gigantic case in Process_Defining_Name, that
   -- calls Check with a list of keys, in order of decreasing generality.
   --
   -- Check calls Check_One_Key for each of the keys, in order of increasing generality (i.e. reverse),
   -- due to "others" filters that are not considered if a more specialized filter exists.
   --
   -- The core of the algorith is in Check_One_Key. The hierarchy between the filters for a given key
   -- is established by checking first with a type or else with a category or else without anything.
   -- Unlike types/categories that make a hierarchy, the visibility parameter of a filter is just used
   -- to determine if a filter is applicable or not.
   --
   -- The checking process first determines if a filter is applicable, then (if applicable) checks
   -- negative patterns (which immediately trigger an error if matched), then checks the positive patterns.
   -- That's where it gets more complicated: it is an error if no positive pattern is matched for any
   -- applicable filter, unless there is no positive pattern at all among applicable patterns. This is
   -- managed through various flags:
   --   - Global_Applicable_Found: (the only one that runs through all keys) is true once an applicable
   --     filter has been found; it means that upper "others" filter are no more considered
   --   - Appropriate_Found: An appropriate filter has been found. If it has any positive filter, it is an
   --     error if there is no match.
   --   - Positive_Found: An appropriate filter with positive patterns has been found.
   --   - Positive_Matched: A positive pattern has been matched.
   --   - Root_Found: At least one of the filters is a "root" filter
   --
   -- Note that the matching process is stopped as soon as a certain error is diagnosed, or a certain
   -- OK is diagnosed (the latter can happen early only when a "root" filter is encountered). This is
   -- done by raising an exception, since it can happen deep in the call tree.
   --
   -- This algorithm is what seems to give the most "natural" behaviour, but we may have to refine it
   -- in the future.


   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   -- Here are all keywords for classifying naming conventions
   -- To add a new one, just add it to the type Keys, and insert it at the appropriate places
   -- in the calls to Check in the Process_XXX procedures.
   -- Presentation reflects the hierarchy of naming conventions
   type Keys is (K_Not_A_Key,   -- Not allowed to user
                 K_All,
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
                          K_Regular_Static_Constant,
                          K_Regular_Nonstatic_Constant,
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
   subtype Object_Keys   is Keys range K_Variable .. K_Entry_Index;
   subtype Function_Keys is Keys range K_Function .. K_Generic_Formal_Function;

   package Keys_Flags_Utilities is new Framework.Language.Flag_Utilities (Keys, "K_");
   use Keys_Flags_Utilities;

   type Key_Set_Index is range 1 .. Max_Hierarchy_Depth;
   type Key_Set is array (Key_Set_Index range <>) of Keys;

   type Visibility is (Scope_Global, Scope_Local, Scope_Unit);
   package Visibility_Utilities is new Framework.Language.Modifier_Utilities (Modifiers  => Visibility,
                                                                              Prefix     => "SCOPE_");
   subtype Scope_Set is Visibility_Utilities.Modifier_Set;

   type Usage_Filter_Kind is (None, Category, Entity);
   type Naming_Context (Filter_Kind : Usage_Filter_Kind) is new Basic_Rule_Context with
      record
         Scopes            : Scope_Set;
         Is_Root           : Boolean := False;
         Is_Others         : Boolean := False;
         Positive_Patterns : Pattern_Queues.Queue;
         Negative_Patterns : Pattern_Queues.Queue;
         case Filter_Kind is
            when None =>
               null;
            when Category =>
               Categories : Framework.Language.Shared_Keys.Categories_Utilities.Modifier_Set;
            when Entity =>
               Spec : Entity_Specification;
         end case;
      end record;
   overriding procedure Clear (Context : in out Naming_Context);

   Contexts : Context_Store;
   package Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Contexts);

   Usage : array (Keys) of Boolean := (others => False);

   No_More_Checks : exception;

   -- Rule variables
   Default_Case_Sensitivity : aliased Switch_Type.Object := (Value => Off);

   Expected_Categories : constant Categories_Set := Basic_Set + Cat_Private;

   -----------
   -- Clear --
   -----------

   procedure Clear (Context : in out Naming_Context) is
      use Pattern_Queues;
   begin
      Clear (Context.Positive_Patterns);
      Clear (Context.Negative_Patterns);
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
      User_Message  ("Parameter(1): [root] [others] {<location>} [<type_spec>]");
      Help_On_Flags ("                ", Extra_Value => "");
      User_Message  ("Parameter(2..): [case_sensitive|case_insensitive] [not] "
                     & """<name pattern>""|file ""<file name>""");
      Visibility_Utilities.Help_On_Modifiers  (Header => "<location> :");
      User_Message  ("<type_spec>: <entity> | {<category>}");
      Help_On_Categories (Expected => Expected_Categories);
      User_Message;
      User_Message  ("Variables:");
      Help_On_Variable (Rule_Id & ".Default_Case_Sensitivity");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Characters.Handling, Ada.Exceptions, Framework.Language, String_Matching;
      use Visibility_Utilities;
      use Categories_Utilities;

      procedure Append_From_File (Pat : in out Pattern_Queues.Queue; Ignore_Case : Boolean; Name : Wide_String) is
         use Ada.Wide_Characters.Handling, Ada.Wide_Text_IO;
         use Pattern_Queues, Utilities;

         Name_File : Ada.Wide_Text_IO.File_Type;
      begin
         Open (Name_File,
               In_File,
               To_String (Name),
               Form => Implementation_Options.Form_Parameters);

         On_Lines : loop -- exit on End_Error, works with malformed files
            declare
               Line     : constant Wide_String := Get_Line (Name_File);
               Start    : Natural := Line'First;  -- Points at beginning of identifier
               Stop     : Natural;                -- Points after identifier
            begin
               while Start <= Line'Last  loop
                  if Is_Space (Line (Start)) or To_Character (Line (Start)) = Ada.Characters.Latin_1.HT then
                     -- Separator
                     Start := Start + 1;

                  elsif Start < Line'Last  and then (Line (Start) = '-' and Line (Start + 1) = '-') then
                     -- Comment
                     exit;

                  elsif Is_Alphanumeric (Line (Start)) then
                     -- Simple identifier
                     Stop := Start + 1;
                     while Stop <= Line'Last
                       and then (Is_Letter (Line (Stop)) or Is_Punctuation_Connector (Line (Stop)))
                     loop
                        Stop := Stop + 1;
                     end loop;
                     Append (Pat, Compile ('^' & Line (Start .. Stop - 1) & '$', Ignore_Case));
                     Start := Stop;

                  elsif Line (Start) = '"' then
                     -- Pattern
                     Start := Start + 1;
                     Stop  := Start + 1;
                     while Stop <= Line'Last and then Line (Stop) /= '"' loop
                        -- No need to handle "" specially, since it is not allowed in an identifier and is not
                        -- a pattern special character
                        Stop := Stop + 1;
                     end loop;
                     if Stop <= Line'Last then
                        Append (Pat, Compile (Line (Start .. Stop - 1), Ignore_Case));
                        Start := Stop + 1;
                     else
                        Parameter_Error (Rule_Id, "Missing terminating quote for pattern at "
                                         & Name
                                         & ':' & Integer_Img (Integer (Ada.Wide_Text_IO.Line (Name_File)))
                                         & ':' & Integer_Img (Integer (Col (Name_File))));
                     end if;
                  else
                     Parameter_Error (Rule_Id,
                                      "Invalid character in name file at "
                                      & Name
                                      & ':' & Integer_Img (Integer (Ada.Wide_Text_IO.Line (Name_File)))
                                      & ':' & Integer_Img (Integer (Col (Name_File))));
                  end if;
               end loop;
            end;
         end loop On_Lines;

      exception
         -- Note: there can be no Pattern_Error, since we allow only identifier characters
         when Name_Error =>
            Parameter_Error (Rule_Id, "Name file """ & Name & """ not found");
         when End_Error =>
            Close (Name_File);
         when others =>
            if Is_Open (Name_File) then
               Close (Name_File);
               raise;
            end if;
      end Append_From_File;


      Key        : Keys;
      Scopes     : Scope_Set;
      Categories : Categories_Utilities.Modifier_Set;
      Is_Root    : Boolean;
      Is_Others  : Boolean;
      Spec       : Entity_Specification;
      Kind       : Usage_Filter_Kind;
   begin   -- Add_Control
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "kind of filter required");
      end if;

      Is_Root   := Get_Modifier ("ROOT");
      Is_Others := Get_Modifier ("OTHERS");

      Scopes    := Visibility_Utilities.Get_Modifier_Set;
      if Scopes = Visibility_Utilities.Empty_Set then
         Scopes := Visibility_Utilities.Full_Set;
      end if;

      Categories := Get_Modifier_Set (Expected => Expected_Categories);
      if Categories = Categories_Utilities.Empty_Set then
         Key := Get_Flag_Parameter (Allow_Any => True);
         if Key = K_Not_A_Key then   -- Presumably an entity
            Kind := Entity;
            Spec := Get_Entity_Modifier;
            Key  := Get_Flag_Parameter (Allow_Any => True);
         else
            Kind := None;
         end if;
      else
         for C in Categories'Range loop
            if Categories (C) and not Expected_Categories (C) then
               Parameter_Error (Rule_Id, "Category not allowed: " & Image (C));
            end if;
         end loop;
         Kind := Category;
         Key  := Get_Flag_Parameter (Allow_Any => True);
      end if;

      if Key = K_Not_A_Key then
         Parameter_Error (Rule_Id, "Filter expected");
      elsif Kind /= None and Key not in Object_Keys and Key not in Function_Keys then
         Parameter_Error (Rule_Id, "categories or entity allowed only for variables and constants");
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "at least one pattern required");
      end if;

      declare
         use Pattern_Queues;
         Cont : Naming_Context (Kind);
      begin
         case Kind is
            when  None =>
               Cont := (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                        Filter_Kind    => None,
                        Scopes         => Scopes,
                        Is_Root        => Is_Root,
                        Is_Others      => Is_Others,
                        Positive_Patterns => Empty_Queue,
                        Negative_Patterns => Empty_Queue);
            when Category =>
               Cont := (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                        Filter_Kind    => Category,
                        Scopes         => Scopes,
                        Is_Root        => Is_Root,
                        Is_Others      => Is_Others,
                        Positive_Patterns => Empty_Queue,
                        Negative_Patterns => Empty_Queue,
                        Categories     => Categories);
            when Entity =>
               Cont := (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                        Filter_Kind    => Entity,
                        Scopes         => Scopes,
                        Is_Root        => Is_Root,
                        Is_Others      => Is_Others,
                        Positive_Patterns => Empty_Queue,
                        Negative_Patterns => Empty_Queue,
                        Spec           => Spec);
         end case;

         while Parameter_Exists loop
            declare
               Ignore_Case : constant Boolean     := Get_Modifier (True_KW  => "CASE_INSENSITIVE",
                                                                   False_KW => "CASE_SENSITIVE",
                                                                   Default  => Default_Case_Sensitivity.Value = Off);
               Is_Not      : constant Boolean     := Get_Modifier ("NOT");
               Is_File     : constant Boolean     := Get_Modifier ("FILE");
            begin
               if Is_File then
                  if Is_Not then
                     Append_From_File (Cont.Negative_Patterns, Ignore_Case, Get_File_Parameter);
                  else
                     Append_From_File (Cont.Positive_Patterns, Ignore_Case, Get_File_Parameter);
                  end if;
               else
                  declare
                     Pattern : constant Wide_String := Get_String_Parameter;
                  begin
                     if Is_Not then
                        Append (Cont.Negative_Patterns, Compile (Pattern, Ignore_Case));
                     else
                        Append (Cont.Positive_Patterns, Compile (Pattern, Ignore_Case));
                     end if;
                  exception
                     when Occur : Pattern_Error =>
                        Parameter_Error (Rule_Id,
                                         "Incorrect pattern: " & Pattern
                                         & " (" & To_Wide_String (Exception_Message (Occur)) & ')');
                  end;
               end if;
            end;
         end loop;

         Associate (Contexts,
                    Value (Image (Key) & '_' & Usage_Filter_Kind'Wide_Image (Kind)),
                    Cont,
                    Additive => True);
      exception
         when Already_In_Store =>
            Parameter_Error (Rule_Id, "These filters already given");
      end;

      Rule_Used   := True;
      Usage (Key) := True;
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
            Clear (Contexts);
            Usage                    := (others => False);
            Default_Case_Sensitivity := (Value => Off);
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
     (for some F of Filters => Usage (F));

   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Name : in Asis.Defining_Name) is
      use Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Utilities;
      Decl          : Asis.Declaration; -- The declaration on which we base the classification

      -- Applicable rules must be given in Set in order of decreasing generality
      -- For objects (constants and variables) and functions, Object_Type is an identifier or definition of its type,
      --     otherwise, it is Nil_Element
      procedure Check (Name_Str        : in Wide_String;
                       Set             : in Key_Set;
                       Object_Category : in Categories   := Cat_Any;
                       Object_Type     : in Asis.Element := Asis.Nil_Element)
      is
         use Asis.Limited_Views;
         use Scope_Manager;

         Is_Program_Unit         : constant Boolean := Is_Equal (Decl, Current_Scope);
         Is_Compilation_Unit     : constant Boolean := Is_Program_Unit and Current_Depth = 1;
         Is_Global               : Boolean;
         Scopes_Mask             : Scope_Set;
         Good_Cat                : Categories := Object_Category;
         Global_Applicable_Found : Boolean := False;
         Last_Position           : Iterator_Position;
         Good_Object_Type        : constant Asis.Element := (if Is_From_Limited_View (Object_Type)
                                                             then A4G_Bugs.Get_Nonlimited_View (Object_Type)
                                                             else Object_Type);

         procedure Check_One_Key (Key : in Keys) is
            use Visibility_Utilities, Framework.Locations, Framework.Reports;
            Key_Image         : constant Wide_String := Image (Key, Lower_Case);
            Iter              : Context_Iterator := Iterator.Create;
            Type_Def_Name     : Asis.Declaration;
            First_St_Def_Name : Asis.Declaration;
            Appropriate_Found : Boolean := False;
            Positive_Found    : Boolean := False;
            Positive_Matched  : Boolean := False;
            Root_Found        : Boolean := False;
            OK_For_Type       : Boolean;

            function Filter_Image (Cont : Naming_Context) return Wide_String is
               use Categories_Utilities;
               Scope_Image : constant Wide_String := Image (Cont.Scopes, Default => Visibility_Utilities.Full_Set);
            begin
               case Cont.Filter_Kind is
                  when None =>
                     return Scope_Image & Key_Image;
                  when Category =>
                     return Scope_Image
                       & Image (Good_Cat, Lower_Case) & ' '
                       & Key_Image;
                  when Entity =>
                     return Scope_Image
                       & Image (Cont.Spec) & ' '
                       & Key_Image;
               end case;
            end Filter_Image;

            procedure Check_Context (Cont : Naming_Context) is
               use Framework.Pattern_Queues, Framework.Pattern_Queues_Matchers;
            begin
               if Cont.Is_Others and Global_Applicable_Found then
                  return;
               end if;
               Global_Applicable_Found := True;

               if Match_Any (Name_Str, Cont.Negative_Patterns) then
                  Report (Rule_Id,
                          Cont,
                          Get_Location (Name),
                          "Name does not follow naming rule for """
                          & Filter_Image (Cont)
                          & """: """
                          & Defining_Name_Image (Name) & '"');
                  Reports.Fixes.Refactor (Name);
                  raise No_More_Checks;
               end if;

               if Cont.Positive_Patterns = Empty_Queue or Positive_Matched then
                  -- Rule with no positive pattern => No check
                  -- or already found a matching pattern (but we need to go on to check
                  -- other negative patterns)
                  return;
               end if;

               Positive_Found := True;
               Root_Found     := Root_Found or Cont.Is_Root;
               Save (Iter, Last_Position);
               if Match_Any (Name_Str, Cont.Positive_Patterns) then
                  Positive_Matched := True;
                  if Cont.Is_Root then
                     raise No_More_Checks;
                  end if;
                  return;
               end if;
            end Check_Context;

            use Asis;
         begin   -- Check_One_Key

            -- Check with type
            case Element_Kind (Good_Object_Type) is
               when A_Definition =>
                  if Definition_Kind (Good_Object_Type) = A_Subtype_Indication then
                     Type_Def_Name :=  Corresponding_Name_Definition (Simple_Name
                                                                      (Strip_Attributes
                                                                         (Subtype_Simple_Name (Good_Object_Type))));
                     if Is_From_Limited_View (Type_Def_Name) then
                        Type_Def_Name := A4G_Bugs.Get_Nonlimited_View (Type_Def_Name);
                     end if;
                     OK_For_Type := True;
                  else
                     -- Other Definition_Kinds are for anonymous types, cannot match an entity_specification
                     OK_For_Type := False;
                  end if;
               when A_Defining_Name =>
                  Type_Def_Name := Good_Object_Type;
                  OK_For_Type   := True;
               when An_Expression =>  -- Should be an identifier
                  Type_Def_Name := Corresponding_Name_Definition (Simple_Name (Strip_Attributes (Good_Object_Type)));
                  if Is_From_Limited_View (Type_Def_Name) then
                     Type_Def_Name := A4G_Bugs.Get_Nonlimited_View (Type_Def_Name);
                  end if;
                  OK_For_Type   := True;
               when Not_An_Element =>
                  OK_For_Type := False;
               when others =>
                  Failure ("Check_One_Key : bad Object_Type", Good_Object_Type);
            end case;

            if OK_For_Type  then
               Reset (Iter, Value (Key_Image & "_ENTITY"));
               while not Is_Exhausted (Iter) loop
                  declare
                     Good_Context : Naming_Context renames Naming_Context (Value (Iter));
                  begin
                     if (Scopes_Mask and Good_Context.Scopes) /= Visibility_Utilities.Empty_Set then
                        if Matches (Good_Context.Spec, Type_Def_Name) then
                           -- Exact subtype
                           Appropriate_Found := True;
                           Check_Context (Good_Context);
                        else
                           -- First subtype (aka type), if different
                           First_St_Def_Name := Names (A4G_Bugs.Corresponding_First_Subtype
                                                       (Enclosing_Element
                                                        (Type_Def_Name))) (1);
                           if not Is_Equal (First_St_Def_Name, Type_Def_Name)
                             and then Matches (Good_Context.Spec, First_St_Def_Name)
                           then
                              Appropriate_Found := True;
                              Check_Context (Good_Context);
                           end if;
                        end if;
                     end if;
                  end;
                  Next (Iter);
               end loop;
            end if;

            -- Check with category
            if not Appropriate_Found and Good_Cat /= Cat_Any then
               Reset (Iter, Value (Key_Image & "_CATEGORY"));
               while not Is_Exhausted (Iter) loop
                  declare
                     Good_Context : Naming_Context renames Naming_Context (Value (Iter));
                  begin
                     if (Scopes_Mask and Good_Context.Scopes) /= Visibility_Utilities.Empty_Set
                       and then Good_Context.Categories (Good_Cat)
                     then
                        Appropriate_Found := True;
                        Check_Context (Good_Context);
                     end if;
                  end;
                  Next (Iter);
               end loop;
            end if;

            -- Check without type/category
            if not Appropriate_Found then
               Reset (Iter, Value (Key_Image & "_NONE"));
               while not Is_Exhausted (Iter) loop
                  declare
                     Good_Context : Naming_Context renames Naming_Context (Value (Iter));
                  begin
                     if (Scopes_Mask and Good_Context.Scopes) /= Visibility_Utilities.Empty_Set then
                        Appropriate_Found := True;
                        Check_Context (Good_Context);
                     end if;
                  end;
                  Next (Iter);
               end loop;
            end if;

            -- Error if there were positive patterns, but none matched
            if Positive_Found and not Positive_Matched then
               Report (Rule_Id,
                       Naming_Context (Value (Last_Position)),
                       Get_Location (Name),
                       "Name does not follow naming rule for """
                       & Filter_Image (Naming_Context (Value (Last_Position)))
                       & """: """
                       & Defining_Name_Image (Name) & '"');
               Reports.Fixes.Refactor (Name);
               if Root_Found then
                  raise No_More_Checks;
               end if;
            end if;
         end Check_One_Key;

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

         if Good_Cat = Cat_Any and not Is_Nil (Good_Object_Type) then
            Good_Cat := Matching_Category  (Good_Object_Type,
                                            From_Cats => Categories_Utilities.Modifier_Set'
                                                          (Cat_New | Cat_Extension => False,
                                                           others                  => True),
                                            Follow_Derived     => True,
                                            Privacy            => Stop_At_Private,
                                            Separate_Extension => True);
         end if;
         for K : Keys of reverse Set loop
            if Usage (K) then
               Check_One_Key (K);
            end if;
         end loop;

      exception
         when No_More_Checks =>
            null;
      end Check;

      use Asis, Asis.Definitions;
   begin    -- Process_Defining_Name
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Name_Str      : constant Wide_String := Defining_Name_Image (Name);
         Renamed       : Asis.Element := Nil_Element;
         Original_Decl : Asis.Declaration; -- The (true) enclosing declaration of Name;
                                           -- it can differ from decl in cases like renaming
         Decl_Kind     : Asis.Declaration_Kinds;
         Def           : Asis.Definition;
         Accessed      : Asis.Element;

         function Accessed_Kind (Expr : Asis.Expression) return Asis.Declaration_Kinds is
         -- Expr is an expression of an access type
         -- Finds the Declaration_Kind of the accessed element
            Renamed_T : Asis.Definition;
         begin
            Renamed_T := Thick_Queries.Corresponding_Expression_Type_Definition (Expr);
            if Definition_Kind (Renamed_T) = An_Access_Definition then
               -- anonymous access type
               -- 2005 can occur in many declarations...
               case Access_Definition_Kind (Renamed_T) is
                  when Not_An_Access_Definition =>
                     Failure ("Process_Defining_Name: Not an access definition", Renamed_T);
                  when An_Anonymous_Access_To_Variable =>
                     return A_Variable_Declaration;
                  when An_Anonymous_Access_To_Constant =>
                     return A_Constant_Declaration;
                  when others =>
                     Failure ("Process_Defining_Name: Unexpected dereference", Expr);
               end case;
            else
               -- regular access type
               case Access_Type_Kind (Renamed_T) is
                  when A_Pool_Specific_Access_To_Variable
                     | An_Access_To_Variable
                     =>
                     return A_Variable_Declaration;
                  when An_Access_To_Constant =>
                     return A_Constant_Declaration;
                  when others =>
                     Failure ("Process_Defining_Name: Unexpected named dereference", Expr);
               end case;
            end if;
         end Accessed_Kind;

      begin
         Decl := Enclosing_Element (Name);
         while Defining_Name_Kind (Decl) = A_Defining_Expanded_Name loop
            -- Name was the name of a child compilation unit
            Decl := Enclosing_Element (Decl);
         end loop;
         Original_Decl := Decl;

         -- Every path in the following case statement must end with a call to Check,
         -- and perform nothing after.
         -- There is nothing after the case statement.
         -- It is therefore irrelevant whether the call to Check is followed by a return or not.
         -- In most cases, there is no return, however in some cases there is a return when the
         -- call to check is deep in the statements, to simplify the structure.
         case Element_Kind (Decl) is
            when A_Statement =>  ------------------------------------------------- Statements
               for L : Asis.Defining_Name of Label_Names (Decl) loop
                  if Is_Equal (Name, L) then
                     Check (Name_Str, (K_All, K_Label));
                     return;
                  end if;
               end loop;

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

               if Decl_Kind in A_Full_Type_Declaration
                 and then Declaration_Kind (Corresponding_Type_Partial_View (Decl))
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
                                          Renamed := Selector (Renamed);  -- Because fields have their own convention
                                       when A_Slice
                                          | An_Indexed_Component
                                            =>
                                          Renamed := Prefix (Renamed);    -- Going up to check variable or constant

                                          -- Beware! It might be an implicit dereference
                                          if Is_Access_Expression (Renamed) then
                                             Decl_Kind := Accessed_Kind (Renamed);
                                             Decl := Nil_Element;  -- Renamed element declaration is unknown (dynamic)
                                             exit Going_Up_Renamings;
                                          end if;
                                       when A_Function_Call =>
                                          Decl      := Nil_Element;  -- Renamed element declaration is unknown (dynamic)
                                          Decl_Kind := A_Constant_Declaration;
                                          exit Going_Up_Renamings;
                                       when A_Type_Conversion
                                          | A_Qualified_Expression =>
                                          Renamed := Converted_Or_Qualified_Expression (Renamed);
                                       when An_Identifier
                                          | An_Enumeration_Literal
                                          | A_Character_Literal
                                            =>
                                          exit;
                                       when An_Explicit_Dereference =>
                                          Decl_Kind := Accessed_Kind (Prefix (Renamed));
                                          Decl := Nil_Element;  -- Renamed element declaration is unknown (dynamic)
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
                        Decl      := A4G_Bugs.Corresponding_First_Subtype (Decl);
                        Decl_Kind := Declaration_Kind (Decl);
                        Def       := Type_Declaration_View (Decl);
                        if Type_Kind (Def) in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition then
                           -- Subtype of a derived type
                           Decl      := Corresponding_Root_Type (Def);
                           Decl_Kind := Declaration_Kind (Decl);
                        end if;
                     end if;

                  when An_Incomplete_Type_Declaration | A_Tagged_Incomplete_Type_Declaration =>
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
                           if Declaration_Kind (Accessed)
                              in An_Incomplete_Type_Declaration .. A_Tagged_Incomplete_Type_Declaration
                           then
                              Accessed := Corresponding_Full_Type_Declaration (Accessed);
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
                              Accessed := A4G_Bugs.Corresponding_First_Subtype (Accessed);
                              if Declaration_Kind (Accessed) = A_Formal_Incomplete_Type_Declaration then
                                 exit;
                              end if;
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
                              when A_Formal_Incomplete_Type_Declaration =>
                                 if Has_Tagged (Accessed) then
                                    Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Tagged_Type));
                                 else
                                    Check (Name_Str, (K_All, K_Type, K_Access_Type, K_Access_To_Regular_Type));
                                 end if;
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

                  when An_Incomplete_Type_Declaration | A_Tagged_Incomplete_Type_Declaration =>
                     Failure ("Unexpected incomplete declaration", Decl);

                  when A_Private_Type_Declaration =>
                     Check (Name_Str, (K_All, K_Type, K_Private_Type));

                  when A_Private_Extension_Declaration =>
                     Check (Name_Str, (K_All, K_Type, K_Private_Type, K_Private_Extension));

                  when A_Formal_Type_Declaration =>
                     Check (Name_Str, (K_All, K_Type, K_Generic_Formal_Type));

                  when An_Enumeration_Literal_Specification =>
                     Check (Name_Str, (K_All, K_Constant, K_Enumeration), Object_Category => Cat_Enum);

                  when A_Variable_Declaration =>  ------------------------ Constants, Variables, Parameters
                     Check (Name_Str,
                            (K_All, K_Variable, K_Regular_Variable),
                            Object_Type => Object_Declaration_View (Original_Decl));

                  when A_Constant_Declaration =>
                     -- Decl is Nil_Element in the case of a renaming of a dereference => dynamic
                     -- but Decl_Kind is correct
                     if Is_Nil (Decl) then
                        Check (Name_Str,
                               (K_All, K_Constant, K_Regular_Constant, K_Regular_Nonstatic_Constant),
                               Object_Type => Object_Declaration_View (Original_Decl));
                     elsif Is_Static_Expression (Initialization_Expression (Decl), RM_Static => True) then
                        Check (Name_Str,
                               (K_All, K_Constant, K_Regular_Constant, K_Regular_Static_Constant),
                               Object_Type => Object_Declaration_View (Original_Decl));
                     else
                        Check (Name_Str,
                               (K_All, K_Constant, K_Regular_Constant, K_Regular_Nonstatic_Constant),
                               Object_Type => Object_Declaration_View (Original_Decl));
                     end if;

                  when A_Deferred_Constant_Declaration =>
                     declare
                        Good_Decl : Asis.Declaration;
                     begin
                        if Is_Nil (Renamed) then
                           Good_Decl := Corresponding_Constant_Declaration (Name);
                        else
                           Good_Decl := Corresponding_Constant_Declaration (Corresponding_Name_Definition (Renamed));
                        end if;
                        -- Good_Decl can be Nil_Element if the constant is completed by pragma Import
                        -- Considered non static, since anything can happen on the other side...
                        if not Is_Nil (Good_Decl)
                          and then Is_Static_Expression (Initialization_Expression (Good_Decl), RM_Static => True)
                        then
                           Check (Name_Str,
                                  (K_All, K_Constant, K_Regular_Constant, K_Regular_Static_Constant),
                                  Object_Type => Object_Declaration_View (Original_Decl));
                        else
                           Check (Name_Str,
                                  (K_All, K_Constant, K_Regular_Constant, K_Regular_Nonstatic_Constant),
                                  Object_Type => Object_Declaration_View (Original_Decl));
                        end if;
                     end;

                  when A_Choice_Parameter_Specification =>
                     Check (Name_Str, (K_All, K_Constant, K_Occurrence_Name));
                     -- We don't pass an Object_Type here, short of being able
                     -- to retrieve Ada.Exceptions.Exception_Occurrence

                  when An_Entry_Index_Specification =>
                     Check (Name_Str,
                            (K_All, K_Constant, K_Entry_Index),
                            Object_Type => Range_Ultimate_Name (Specification_Subtype_Definition (Decl)));

                  when A_Loop_Parameter_Specification =>
                     Check (Name_Str,
                            (K_All, K_Constant, K_Loop_Control),
                            Object_Type => Range_Ultimate_Name (Specification_Subtype_Definition (Decl)));

                  when An_Integer_Number_Declaration =>
                     Check (Name_Str, (K_All, K_Constant, K_Named_Number, K_Integer_Number));
                     -- We don't pass an Object_Type here, since it can be used as signed or modular

                  when A_Real_Number_Declaration =>
                     Check (Name_Str, (K_All, K_Constant, K_Named_Number, K_Real_Number));
                     -- We don't pass an Object_Type here, since it can be used as float or fixed

                  when A_Parameter_Specification =>
                     -- Check if it is the "real" declaration of the parameter
                     declare
                        Sp_Decl : constant Asis.Declaration := Enclosing_Element (Decl);
                     begin
                        case Declaration_Kind (Sp_Decl) is
                           when A_Procedure_Declaration
                              | A_Null_Procedure_Declaration
                              | A_Generic_Procedure_Declaration
                              | A_Formal_Procedure_Declaration
                              | A_Function_Declaration
                              | An_Expression_Function_Declaration   -- Ada 2012
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
                              Failure ("Parameter specification not in callable statement (2) ("
                                       & Declaration_Kinds'Wide_Image (Declaration_Kind (Sp_Decl))
                                       & ')',
                                       Sp_Decl);
                        end case;
                     end;

                     case Mode_Kind (Decl) is
                        when A_Default_In_Mode
                          | An_In_Mode
                          =>
                           Check (Name_Str,
                                  (K_All, K_Constant, K_Sp_Formal_In),
                                   Object_Type => Object_Declaration_View (Original_Decl));
                        when An_Out_Mode =>
                           Check (Name_Str,
                                  (K_All, K_Variable, K_Procedure_Formal_Out),
                                  Object_Type => Object_Declaration_View (Original_Decl));
                        when An_In_Out_Mode =>
                           Check (Name_Str,
                                  (K_All, K_Variable, K_Procedure_Formal_In_Out),
                                  Object_Type => Object_Declaration_View (Original_Decl));
                        when Not_A_Mode =>
                           Failure ("Unexpected mode: " & Mode_Kinds'Wide_Image (Mode_Kind (Decl)));
                     end case;

                  when A_Formal_Object_Declaration =>
                     case Mode_Kind (Decl) is
                        when A_Default_In_Mode
                          | An_In_Mode
                          =>
                           Check (Name_Str,
                                  (K_All, K_Constant, K_Generic_Formal_In),
                                  Object_Type => Object_Declaration_View (Original_Decl));
                        when An_In_Out_Mode =>
                           Check (Name_Str,
                                  (K_All, K_Variable, K_Generic_Formal_In_Out),
                                  Object_Type => Object_Declaration_View (Original_Decl));
                        when An_Out_Mode
                          | Not_A_Mode
                          =>
                           Failure ("Unexpected mode: " & Mode_Kinds'Wide_Image (Mode_Kind (Decl)));
                     end case;

                  when A_Discriminant_Specification =>
                     Check (Name_Str,
                            (K_All, K_Variable, K_Field, K_Discriminant),
                            Object_Type => Object_Declaration_View (Original_Decl));

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
                           Check (Name_Str,
                                  (K_All, K_Variable, K_Field, K_Protected_Field),
                                  Object_Type => Component_Definition_View (Object_Declaration_View (Decl)));
                        when A_Record_Definition | A_Private_Extension_Definition | A_Null_Record_Definition =>
                           Check (Name_Str,
                                  (K_All, K_Variable, K_Field, K_Record_Field),
                                  Object_Type => Component_Definition_View (Object_Declaration_View (Decl)));
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
                     | An_Expression_Function_Declaration   -- Ada 2012
                     =>
                     if Definition_Kind (Enclosing_Element (Decl)) = A_Protected_Definition then
                        Check (Name_Str,
                               (K_All, K_Subprogram, K_Function, K_Protected_Function),
                               Object_Type => Result_Profile (Decl));
                     else
                        Check (Name_Str,
                               (K_All, K_Subprogram, K_Function, K_Regular_Function),
                               Object_Type => Result_Profile (Decl));
                     end if;

                  when A_Function_Instantiation =>
                        Check (Name_Str,
                               (K_All, K_Subprogram, K_Function, K_Regular_Function),
                               Object_Type => Result_Profile (Corresponding_Declaration (Decl)));

                  when A_Formal_Function_Declaration =>
                     Check (Name_Str,
                            (K_All, K_Subprogram, K_Function, K_Generic_Formal_Function),
                            Object_Type => Result_Profile (Decl));

                  when A_Function_Body_Declaration
                     | A_Function_Body_Stub
                     =>
                     -- Check body only if there is no explicit spec
                     if Is_Nil (Corresponding_Declaration (Decl)) then
                        Check (Name_Str,
                               (K_All, K_Subprogram, K_Function),
                               Object_Type => Result_Profile (Decl));
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
                     Check (Name_Str,
                            (K_All, K_Variable, K_Task, K_Task_Object),
                            Object_Category => Cat_Task);

                  when A_Protected_Type_Declaration =>  ------------------------ Protected
                     Check (Name_Str, (K_All, K_Protected, K_Protected_Type));

                  when A_Single_Protected_Declaration =>
                     Check (Name_Str,
                            (K_All, K_Variable, K_Protected, K_Protected_Object),
                            Object_Category => Cat_Protected);

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
