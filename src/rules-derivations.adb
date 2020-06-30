----------------------------------------------------------------------
--  Rules.Derivations - Package body                                --
--                                                                  --
--  This software  is (c) Adalog  2004-2016.                        --
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

-- Asis
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;

-- Asis for Gnat
with
  Asis.Extensions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Control_Manager,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Reports.Fixes;
pragma Elaborate (Framework.Language);
pragma Elaborate (Framework.Language.Shared_Keys);

package body Rules.Derivations is
   use Framework, Framework.Control_Manager, Framework.Language, Framework.Language.Shared_Keys;

   -- Algorithm
   --
   -- From: we simply walk up the chain of derivations and progenitors, checking the parent type,
   -- until we encounter something that's not a derived type.
   --
   -- Indicator: we start from every subprogram declaration. We determine whether it is primitive, for
   -- a tagged or untagged derived type, whether it is a specification, a body acting as a declaration, or
   -- a true body. We cross this with the required checks and whether an indicator is given.
   -- This is quite simple... The whole difficulty lies in the services dealing with primitive operations in
   -- Thick_Queries!
   --
   -- Max_Parent: count the progenitors, +1 for derived types (but not task, protected, and interfaces)
   --
   -- Max_Depth: count the depth of the greatest ancestor, +1 for derived types

   type Subrules is (SR_From, SR_Indicator, SR_Max_Depth, SR_Max_Parents);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "SR_");
   type Subrules_Set is array (Subrules) of Boolean;
   No_Rule : constant Subrules_Set := (others => False);

   Rule_Used : Subrules_Set := No_Rule;
   Save_Used : Subrules_Set;

   -- Data for subrules From and Max_Parents:
   type Maxima is array (Control_Kinds) of Thick_Queries.Biggest_Natural;
   Uninitialized : constant Thick_Queries.Biggest_Natural := Thick_Queries.Biggest_Natural'Last;
   type Value_Context is new Basic_Rule_Context with
      record
         Maximum : Maxima := (others => Uninitialized);
      end record;

   Searched_Parents : Context_Store;

   From_Expected_Categories : constant Categories_Set := Basic_Set + Cat_Private;


   -- Data for subrule Indicator :
   type Type_Gender        is (Gender_Tagged, Gender_Untagged);
   type Indicator_Gender   is (Gender_Overriding, Gender_Not_Overriding);
   type Declaration_Gender is (Gender_Declaration, Gender_Body);
   type Gender_Action      is (Required, Forbidden, Unchecked);

   Indicator_Uses     : array (Type_Gender, Indicator_Gender, Declaration_Gender) of Gender_Action
                        := (others => (others => (others => Unchecked)));
   Indicator_Contexts : array (Type_Gender, Indicator_Gender, Declaration_Gender) of Basic_Rule_Context;

   -- Data for subrule Max_Depth :
   type Depth_Filter is (DF_Tagged, DF_Untagged, DF_Task, DF_Protected);
   package Filter_Modifiers is new Modifier_Utilities (Modifiers => Depth_Filter,
                                                       Prefix    => "DF_");
   subtype Filter_Set is Filter_Modifiers.Modifier_Set;

   type Depth_Value_Context is new Basic_Rule_Context with
      record
         Maximum : Thick_Queries.Biggest_Natural := Uninitialized;
      end record;

   Searched_Depth : array (Control_Kinds, Depth_Filter) of Depth_Value_Context;
   Depth_Used     : array (Control_Kinds, Depth_Filter) of Boolean := (others => (others => False));


   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
      use Subrules_Flag_Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls ancestors and primitives of a derived type");
      User_Message;
      Help_On_Flags ("Parameter(1):");
      User_Message;
      User_Message ("for From subrule:");
      User_Message ("Parameter(2..): <Entity name> | <category>");
      Help_On_Categories (Expected => From_Expected_Categories);
      User_Message;
      User_Message ("for Indicator subrule:");
      User_Message ("Parameter(2): tagged | untagged (optional)");
      User_Message ("Parameter(3): overriding | not_overriding (optional)");
      User_Message ("Parameter(4): declaration | body_required | body_forbidden (optional)");
      User_Message;
      User_Message ("for Max_Depth subrule:");
      User_Message ("Parameter(2): [tagged | untagged | task | protected] <value>");
      User_Message;
      User_Message ("for Max_Parents subrule:");
      User_Message ("Parameter(2): <value>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Subrules_Flag_Utilities;

      Subrule_Name : Subrules;
      Bound        : Thick_Queries.Biggest_Natural;
      Type_Given   : array (Type_Gender)        of Boolean := (others => False);
      Indic_Given  : array (Indicator_Gender)   of Boolean := (others => False);
      Decl_Given   : array (Declaration_Gender) of Boolean := (others => False);
      Body_Action  : Gender_Action;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;

      Subrule_Name := Get_Flag_Parameter (Allow_Any => False);
      case Subrule_Name is
         when SR_From =>
            loop
               declare
                  Entity : constant Entity_Specification := Get_Entity_Parameter (Allow_Extended => Parens_OK);
               begin
                  Check_Category (Rule_Id, Entity, From_Expected_Categories);
                  Associate (Searched_Parents,
                             Entity,
                             Value_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label)
                               with Maximum => (others => Uninitialized)));
               exception
                  when Already_In_Store =>
                     Parameter_Error (Rule_Id, "entity already given: " & Image (Entity));
               end;
               exit when not Parameter_Exists;
            end loop;

         when SR_Max_Parents =>
            declare
               use Thick_Queries;
               Key  : constant Entity_Specification := Value ("ALL");
               Cont : Value_Context := (Basic.New_Context (Ctl_Kind, Ctl_Label) with (others => Uninitialized));
            begin
               Bound := Get_Integer_Parameter (Min => 1);
               Cont.Maximum (Ctl_Kind) := Bound;
               Associate (Searched_Parents, Key, Cont);
            exception
               when Already_In_Store =>
                  Cont := Value_Context (Association (Searched_Parents, Key));
                  if Cont.Maximum (Ctl_Kind) /= Uninitialized then
                     Parameter_Error (Rule_Id, "Maximum parents already given");
                  end if;
                  Cont.Maximum (Ctl_Kind) := Bound;
                  Update (Searched_Parents, Cont);
            end;

         when SR_Max_Depth =>
            declare
               use Filter_Modifiers;
               Filters : Filter_Set;
            begin
               Filters := Filter_Modifiers.Get_Modifier_Set;
               Bound   := Get_Integer_Parameter (Min => 0);
               if Filters = Filter_Modifiers.Empty_Set then
                  -- No gender, so all gender
                  for Df in Depth_Filter loop
                     if Depth_Used (Ctl_Kind, Df) then
                        Parameter_Error (Rule_Id, "Maximum depth already given for " & Image (Df)
                                         & ' ' & Control_Kinds'Wide_Image (Ctl_Kind));
                     else
                        Depth_Used     (Ctl_Kind, Df) := True;
                        Searched_Depth (Ctl_Kind, Df) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Bound);
                     end if;
                  end loop;
                  Filters := Filter_Modifiers.Full_Set;
               else
                  for Gender in Depth_Filter loop
                     if Filters (Gender) then
                        if Depth_Used (Ctl_Kind, Gender) then
                           Parameter_Error (Rule_Id, "Maximum depth already given for "
                                            & Depth_Filter'Wide_Image (Gender)
                                            & ' ' & Control_Kinds'Wide_Image(Ctl_Kind));
                        end if;
                        Depth_Used (Ctl_Kind, Gender)     := True;
                        Searched_Depth (Ctl_Kind, Gender) := (Basic.New_Context (Ctl_Kind, Ctl_Label) with Bound);
                     end if;
                  end loop;
               end if;
            end;

         when SR_Indicator =>
            if Get_Modifier ("TAGGED") then
               Type_Given (Gender_Tagged) := True;
               Get_Null_Parameter;
            elsif Get_Modifier ("UNTAGGED") then
               Type_Given (Gender_Untagged) := True;
               Get_Null_Parameter;
            else
               Type_Given := (others => True);
            end if;

            if Get_Modifier ("OVERRIDING") then
               Indic_Given (Gender_Overriding) := True;
               Get_Null_Parameter;
            elsif Get_Modifier ("NOT_OVERRIDING") then
               Indic_Given (Gender_Not_Overriding) := True;
               Get_Null_Parameter;
            else
               Indic_Given := (others => True);
            end if;

            if Get_Modifier ("DECLARATION") then
               Decl_Given (Gender_Declaration) := True;
               Get_Null_Parameter;
            end if;
            if Get_Modifier ("BODY_REQUIRED") then
               Decl_Given (Gender_Body) := True;
               Body_Action := Required;
               Get_Null_Parameter;
            elsif Get_Modifier ("BODY_FORBIDDEN") then
               Decl_Given (Gender_Body) := True;
               Body_Action := Forbidden;
               Get_Null_Parameter;
            end if;
            if Decl_Given = (Declaration_Gender => False) then
               Decl_Given (Gender_Declaration) := True;
               -- No need to initialize Body_Action since Decl_Given(Gender_Body) is false
            end if;

            for T in Type_Gender loop
               for I in Indicator_Gender loop
                  if Type_Given (T) and Indic_Given (I) and Decl_Given (Gender_Declaration) then
                     if Indicator_Uses (T, I, Gender_Declaration) = Unchecked then
                        Indicator_Uses     (T, I, Gender_Declaration) := Required;
                        Indicator_Contexts (T, I, Gender_Declaration) := Basic.New_Context (Ctl_Kind, Ctl_Label);
                     else
                        Parameter_Error (Rule_Id, "This combination of parameters already given");
                     end if;
                  end if;
                  if Type_Given (T) and Indic_Given (I) and Decl_Given (Gender_Body) then
                     if Indicator_Uses (T, I, Gender_Body) = Unchecked then
                        Indicator_Uses     (T, I, Gender_Body) := Body_Action;
                        Indicator_Contexts (T, I, Gender_Body) := Basic.New_Context (Ctl_Kind, Ctl_Label);
                     else
                        Parameter_Error (Rule_Id, "This combination of parameters already given");
                     end if;
                  end if;
               end loop;
            end loop;
      end case;

      Rule_Used (Subrule_Name) := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := No_Rule;
            Clear (Searched_Parents);
            Indicator_Uses := (others => (others => (others => Unchecked)));
            Depth_Used     := (others => (others => False));
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := No_Rule;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Searched_Parents);
   end Prepare;

   -----------------------------
   -- Derivation_Subtype_Name --
   -----------------------------

   function Derivation_Subtype_Name (Def : Asis.Definition) return Asis.Expression is
      use Asis, Asis.Definitions, Asis.Elements;
      use Thick_Queries, Utilities;
   begin
      case Definition_Kind (Def) is
         when A_Type_Definition =>
            return Subtype_Simple_Name (Parent_Subtype_Indication (Def));
         when A_Private_Extension_Definition =>
            return Subtype_Simple_Name (Ancestor_Subtype_Indication (Def));
         when A_Formal_Type_Definition | A_Task_Definition
            | A_Subtype_Indication
            =>
            return Subtype_Simple_Name (Def);
         when others =>
            Failure ("Derivation_Subtype_Name: bad definition_kind ("
                     & Definition_Kinds'Wide_Image (Definition_Kind (Def)) & ')');
      end case;
   end Derivation_Subtype_Name;

   ---------------------
   -- Check_From_Type --
   ---------------------

   procedure Check_From_Type (Type_Name : Asis.Name; Loc : Locations.Location) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions;
      use Thick_Queries;

      Type_Decl          : Asis.Definition;
      Type_Def : Asis.Definition;

      procedure Check_Name (The_Name : Asis.Name) is

         Parent_Decl : Asis.Declaration;
         Reported       : Boolean;

         procedure Do_Report (Elem : Asis.Element; Message : Wide_String; Found : out Boolean) is
            use Reports;
            Context : constant Root_Context'Class := Matching_Context (Searched_Parents, Elem);
         begin
            Found := Context /= No_Matching_Context;
            if Found then
               Report (Rule_Id,
                       Context,
                       Loc,
                       Message & ' ' & Last_Matching_Name (Searched_Parents));
            end if;
         end Do_Report;
      begin  -- Check_Name
         -- Check parent type
         Do_Report (The_Name, "Type derived from", Reported);
         if Reported then
            return;
         end if;

         Parent_Decl := Corresponding_Name_Declaration (Strip_Attributes (The_Name));

         -- If it is a subtype, check corresponding type
         if Declaration_Kind (Parent_Decl) not in A_Type_Declaration then
            Do_Report (Names (A4G_Bugs.Corresponding_First_Subtype (Parent_Decl)) (1), "Type derived from", Reported);
            if Reported then
               return;
            end if;
         end if;

         -- Check enclosing unit of parent type
         pragma Warnings (Off, Reported); -- GNAT complains that Found is not checked
         Do_Report (Names (Unit_Declaration (Enclosing_Compilation_Unit (Parent_Decl))) (1),
                    "Type derived from type in unit",
                    Reported);
         if Reported then
            return;
         end if;

      end Check_Name;

   begin  -- Check_From_Type
      Check_Name (Type_Name);

      -- Move to upper parent
      Type_Decl := Corresponding_Name_Declaration (Strip_Attributes (Type_Name));
      Type_Def  := Type_Declaration_View (A4G_Bugs.Corresponding_First_Subtype (Type_Decl));
      if        Type_Kind (Type_Def)       in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
        or else Formal_Type_Kind (Type_Def) = A_Formal_Derived_Type_Definition
      then
         Check_From_Type (Derivation_Subtype_Name (Type_Declaration_View (Type_Decl)), Loc);
      end if;

      if        Type_Kind (Type_Def)        = A_Derived_Record_Extension_Definition
        or else Type_Kind (Type_Def)        = An_Interface_Type_Definition
        or else Formal_Type_Kind (Type_Def) = A_Formal_Derived_Type_Definition
      then
         for Progenitor : Asis.Expression of Definition_Interface_List (Type_Def) loop
            Check_From_Type (Simple_Name (Progenitor), Loc);
         end loop;
      end if;
   end Check_From_Type;


   -------------------------
   -- Check_From_Category --
   -------------------------

   procedure Check_From_Category (Type_Name : Asis.Name; Loc : Locations.Location) is
      use Language.Shared_Keys.Categories_Utilities, Reports, Thick_Queries;

      Context : constant Root_Context'Class
        := Control_Manager.Association (Searched_Parents,
                                        Image (Matching_Category (Type_Name,
                                                                  From_Cats          => Full_Set,
                                                                  Follow_Derived     => True,
                                                                  Privacy            => Stop_At_Private,
                                                                  Separate_Extension => False)));
   begin
      if Context /= No_Matching_Context then
         Report (Rule_Id,
                 Context,
                 Loc,
                 "Type derived from category " & Last_Matching_Name (Searched_Parents));
      end if;
   end Check_From_Category;


   -----------------------
   -- Check_Max_Parents --
   -----------------------

   procedure Check_Max_Parents (Element : Asis.Element; Value : Thick_Queries.Biggest_Natural) is
      use Framework.Locations, Framework.Reports, Thick_Queries;

      Context : Value_Context := Value_Context (Association (Searched_Parents, "ALL"));
   begin
      if Value > Context.Maximum (Check) then
         Context.Ctl_Kind := Check;
         Report (Rule_Id,
                 Context,
                 Get_Location (Element),
                 "More than " & Biggest_Int_Img (Context.Maximum (Check))
                 & " parents (" & Biggest_Int_Img (Value) & ')');
      elsif Value > Context.Maximum (Search) then
         Context.Ctl_Kind := Search;
         Report (Rule_Id,
                 Context,
                 Get_Location (Element),
                 "More than " & Biggest_Int_Img (Context.Maximum (Search))
                 & " parents (" & Biggest_Int_Img (Value) & ')');
      end if;

      if Value > Context.Maximum (Count) then
         Context.Ctl_Kind := Count;
         Report (Rule_Id,
                 Context,
                 Get_Location (Element),
                 "");
      end if;
   end Check_Max_Parents;


   ---------------------
   -- Check_Max_Depth --
   ---------------------

   procedure Check_Max_Depth (Element : Asis.Definition)
   is
      use Asis, Asis.Declarations, Asis.Elements;
      use Thick_Queries, Framework.Locations, Framework.Reports;

      type Boolean_Control_Kind_Array is array (Control_Kinds) of Boolean;
      type Depth_Value_Context_Array is array (Control_Kinds) of Depth_Value_Context;

      Ctl_Kind_Depth_Used : Boolean_Control_Kind_Array := (others => False);
      Contexts            : Depth_Value_Context_Array;

      procedure Set_Context (Filter : Depth_Filter) is
      begin
         for Ctl_Kind in Control_Kinds loop
            Ctl_Kind_Depth_Used (Ctl_Kind) := Depth_Used (Ctl_Kind, Filter);
            Contexts (Ctl_Kind)            := Searched_Depth (Ctl_Kind, Filter);
         end loop;
      end Set_Context;

      function Compute_Depth (The_Element : Asis.Definition) return Thick_Queries.Biggest_Natural is
         use Asis.Definitions;
         use Asis.Expressions;
         Max_Value : Thick_Queries.Biggest_Natural := 0;
      begin
         if Definition_Kind (The_Element) = A_Subtype_Indication then
            return Compute_Depth (Type_Declaration_View (Corresponding_Name_Declaration
                                                         (Strip_Attributes (Subtype_Simple_Name (The_Element)))));
         end if;

         case Type_Kind (The_Element) is
            when An_Interface_Type_Definition
               | A_Derived_Record_Extension_Definition =>
               for Progenitor : Asis.Declaration of Definition_Interface_List (The_Element) loop
                  Max_Value := Thick_Queries.Biggest_Natural'Max (Max_Value,
                                                                  Compute_Depth (Type_Declaration_View
                                                                                 (Corresponding_Name_Declaration
                                                                                  (Simple_Name (Progenitor)))) + 1);
               end loop;
            when others =>
               case Definition_Kind (The_Element) is
                  when A_Task_Definition | A_Protected_Definition =>
                     for Progenitor : Asis.Declaration of Declaration_Interface_List (Enclosing_Element (The_Element))
                     loop
                        Max_Value := Thick_Queries.Biggest_Natural'Max (Max_Value,
                                                                        Compute_Depth (Type_Declaration_View
                                                                          (Corresponding_Name_Declaration
                                                                             (Simple_Name (Progenitor)))) + 1);
                     end loop;
                  when others =>
                     null;
               end case;
         end case;

         case Type_Kind (The_Element) is
            when A_Derived_Type_Definition | A_Derived_Record_Extension_Definition =>
               Max_Value := Thick_Queries.Biggest_Natural'Max  (Max_Value,
                                                                Compute_Depth (Parent_Subtype_Indication
                                                                               (The_Element)) + 1);
            when others =>
               null;
         end case;
         return Max_Value;
      end Compute_Depth;

      Depth  : Thick_Queries.Biggest_Natural;
   begin  -- Check_Max_Depth
      case Definition_Kind (Element) is
         when A_Type_Definition =>
            case Type_Kind (Element) is
               when An_Interface_Type_Definition
                  | A_Tagged_Record_Type_Definition
                  | A_Derived_Record_Extension_Definition =>
                  case Interface_Kind (Element) is
                     when A_Protected_Interface =>
                        Set_Context (DF_Protected);
                     when A_Task_Interface =>
                        Set_Context (DF_Task);
                     when A_Synchronized_Interface =>
                        for Ctl_Kind in Control_Kinds loop
                           if Searched_Depth (Ctl_Kind, DF_Protected).Maximum <
                             Searched_Depth (Ctl_Kind, DF_Task).Maximum
                           then
                              Ctl_Kind_Depth_Used (Ctl_Kind) := Depth_Used (Ctl_Kind, DF_Protected);
                              Contexts (Ctl_Kind)            := Searched_Depth (Ctl_Kind, DF_Protected);
                           else
                              Ctl_Kind_Depth_Used (Ctl_Kind) := Depth_Used (Ctl_Kind, DF_Task);
                              Contexts (Ctl_Kind)            := Searched_Depth (Ctl_Kind, DF_Task);
                           end if;
                        end loop;
                     when others =>
                        Set_Context (DF_Tagged);
                  end case;
               when others =>
                  if Is_Type_Declaration_Kind (Element, A_Protected_Type_Declaration) then
                     Set_Context (DF_Protected);
                  elsif Is_Type_Declaration_Kind (Element, A_Task_Type_Declaration) then
                     Set_Context (DF_Task);
                  else
                     Set_Context (DF_Untagged);
                  end if;
            end case;
         when A_Formal_Type_Definition =>
            case Formal_Type_Kind (Element) is
               when A_Formal_Tagged_Private_Type_Definition =>
                  Set_Context (DF_Tagged);
               when others =>
                  Set_Context (DF_Untagged);
            end case;
         when A_Protected_Definition =>
            Set_Context (DF_Protected);
         when A_Task_Definition =>
            Set_Context (DF_Task);
         when others =>
            Set_Context (DF_Untagged);
      end case;

      Depth := Compute_Depth (Element);

      if Ctl_Kind_Depth_Used (Check) and Depth > Contexts (Check).Maximum then
         if Is_Part_Of_Instance (Element) then
            Report (Rule_Id,
                    Contexts (Check),
                    Get_Location (Ultimate_Enclosing_Instantiation (Element)),
                    "Derivation depth greater than " & Biggest_Int_Img (Contexts (Check).Maximum)
                    & " (" & Biggest_Int_Img (Depth) & ") for Declaration at " &
                      Image (Get_Location (Corresponding_Generic_Element (Names (Enclosing_Element (Element)) (1)))));
         else
            Report (Rule_Id,
                    Contexts (Check),
                    Get_Location (Element),
                    "Derivation depth greater than " & Biggest_Int_Img (Contexts (Check).Maximum)
                    & " (" & Biggest_Int_Img (Depth) & ')');
         end if;

      elsif Ctl_Kind_Depth_Used (Search) and Depth > Contexts (Search).Maximum then
         if Is_Part_Of_Instance (Element) then
            Report (Rule_Id,
                    Contexts (Search),
                    Get_Location (Ultimate_Enclosing_Instantiation (Element)),
                    "Derivation depth greater than " & Biggest_Int_Img (Contexts (Search).Maximum)
                    & " (" & Biggest_Int_Img (Depth) & ") for Declaration at " &
                      Image (Get_Location (Corresponding_Generic_Element (Names (Enclosing_Element (Element)) (1)))));
         else
            Report (Rule_Id,
                    Contexts (Search),
                    Get_Location (Element),
                    "Derivation depth greater than " & Biggest_Int_Img (Contexts (Search).Maximum)
                    & " (" & Biggest_Int_Img (Depth) & ')');
         end if;
      end if;

      if Ctl_Kind_Depth_Used (Count) and Depth > Contexts (Count).Maximum then
         if Is_Part_Of_Instance (Element) then
            Report (Rule_Id, Contexts (Count), Get_Location (Ultimate_Enclosing_Instantiation (Element)), "");
         else
            Report (Rule_Id, Contexts (Count), Get_Location (Element), "");
         end if;
      end if;
   end Check_Max_Depth;

   ------------------------
   -- Process_Derivation --
   ------------------------

   procedure Process_Derivation (Element :  Asis.Definition) is
      use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements;
      use Framework.Locations, Thick_Queries;
   begin
      if Rule_Used = No_Rule then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (SR_From) then
         -- Interfaces have progenitors, but no parent...
         if Definition_Kind (Element) not in A_Protected_Definition | A_Task_Definition
           and then Type_Kind (Element) /= An_Interface_Type_Definition
         then
            Check_From_Type     (Derivation_Subtype_Name (Element), Get_Location (Element));
            Check_From_Category (Derivation_Subtype_Name (Element), Get_Location (Element));
         end if;

         -- Untagged derived types have a parent, but no progenitors...
         if Definition_Kind (Element) in A_Protected_Definition | A_Task_Definition then
            for Progenitor : Asis.Expression of Declaration_Interface_List (Enclosing_Element (Element)) loop
               Check_From_Type (Simple_Name (Progenitor), Get_Location (Element));
            end loop;
         elsif not (Type_Kind (Element) in A_Derived_Type_Definition | Not_A_Type_Definition) then
            for Progenitor : Asis.Expression of Definition_Interface_List (Element) loop
               Check_From_Type (Simple_Name (Progenitor), Get_Location (Element));
            end loop;
         end if;
      end if;

      if Rule_Used (SR_Max_Parents) then
         -- Filter those who can have progenitors
         case Type_Kind (Element) is
            when A_Derived_Record_Extension_Definition =>
               Check_Max_Parents (Element, Definition_Interface_List (Element)'Length + 1);
            when An_Interface_Type_Definition =>
               Check_Max_Parents (Element, Definition_Interface_List (Element)'Length);
            when others =>
               null;
         end case;

         case Formal_Type_Kind (Element) is
            when A_Formal_Derived_Type_Definition =>
               Check_Max_Parents (Element, Definition_Interface_List (Element)'Length + 1);
            when A_Formal_Interface_Type_Definition =>
               Check_Max_Parents (Element, Definition_Interface_List (Element)'Length);
            when others =>
               null;
         end case;
      end if;

      if Rule_Used (SR_Max_Depth) then
         -- Filter those who can have progenitors
         case Type_Kind (Element) is
            when A_Derived_Record_Extension_Definition
               | An_Interface_Type_Definition
               | A_Derived_Type_Definition =>
               Check_Max_Depth (Element);
            when others =>
               null;
         end case;

         case Formal_Type_Kind (Element) is
            when A_Formal_Derived_Type_Definition =>
               Check_Max_Depth (Element);
            when A_Formal_Interface_Type_Definition =>
               Check_Max_Depth (Element);
            when others =>
               null;
         end case;

         -- A_Task_Definition
         case Definition_Kind (Element) is
            when A_Protected_Definition | A_Task_Definition =>
               Check_Max_Depth (Element);
            when others =>
               null;
         end case;
      end if;
   end Process_Derivation;

   --------------------------
   -- Process_Synchronized --
   --------------------------

   procedure Process_Synchronized (Sync : Asis.Declaration) is
      use Asis.Declarations;
      use Framework.Locations, Thick_Queries;
   begin
      if not Rule_Used (SR_From) and not Rule_Used (SR_Max_Parents) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Since the category check is only for the parent type, it does not apply to synchronized types.
      declare
         Progenitors : constant Asis.Expression_List := Declaration_Interface_List (Sync);
      begin
         if Rule_Used (SR_From) then
            for Progenitor : Asis.Expression of Progenitors loop
               Check_From_Type (Simple_Name (Progenitor), Get_Location (Sync));
            end loop;
         end if;

         if Rule_Used (SR_Max_Parents) then
            Check_Max_Parents (Sync, Progenitors'Length);
         end if;
      end;
   end Process_Synchronized;

   ----------------------
   -- Process_Callable --
   ----------------------

   Indicator_Table : constant array (Boolean) of Indicator_Gender := (False => Gender_Not_Overriding,
                                                                      True  => Gender_Overriding);

   procedure Process_Callable (Callable : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Extensions;
      use Framework.Locations, Framework.Reports, Framework.Reports.Fixes, Utilities, Thick_Queries;
   begin
      if not Rule_Used (SR_Indicator) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         Primitive_Types : constant Declaration_List := Corresponding_Primitive_Types (Callable);
         Type_G            : Type_Gender;
         Indicator_G       : Indicator_Gender;
         Declaration_G     : Declaration_Gender;
         Derived_Found     : Boolean := False;
         Inst_Or_Call_Decl : Asis.Declaration; -- Instantiation if Callable is from generic, Callable otherwise
      begin
         if Primitive_Types = Nil_Element_List then
            -- Not a primitive operation
            return;
         end if;

         if Is_Part_Of_Instance (Callable) then
            Inst_Or_Call_Decl := Enclosing_Element (Callable);
         else
            Inst_Or_Call_Decl := Callable;
         end if;

         -- Determine type_gender: considered tagged if there is a primitive tagged type.
         -- In some convoluted case, this may not be the type on which the operation is derived,
         -- but it seems reasonable to apply the rule for tagged types as soon as there is a (primitive)
         -- tagged type in the profile.
         Type_G := Gender_Untagged;
         for Prim_Type : Asis.Declaration of Primitive_Types loop
            case Declaration_Kind (Prim_Type) is
               when An_Ordinary_Type_Declaration =>
                  case Type_Kind (Type_Declaration_View (Prim_Type)) is
                     when A_Tagged_Record_Type_Definition =>
                        Type_G := Gender_Tagged;
                     when A_Derived_Record_Extension_Definition =>
                        Derived_Found := True;
                        Type_G        := Gender_Tagged;
                        exit;
                     when A_Derived_Type_Definition =>
                        Derived_Found := True;
                     when others =>
                        null;
                  end case;

               when A_Task_Type_Declaration
                  | A_Single_Task_Declaration
                  | A_Protected_Type_Declaration
                  | A_Single_Protected_Declaration
                  =>
                  -- A task/protected that implements an interface
                  Derived_Found := Declaration_Kind (Callable) /= An_Entry_Declaration;
                  -- Because Asis.Extensions.Is_Overriding_Declaration does not work on entries.
                  -- Replace with True when fixed.
                  Type_G        := Gender_Tagged;
                  exit;

               when A_Private_Type_Declaration =>
                  case Definition_Kind (Type_Declaration_View (Prim_Type)) is
                     when A_Private_Type_Definition =>
                        null;
                     when A_Tagged_Private_Type_Definition =>
                        Type_G := Gender_Tagged;
                     when others =>
                        Failure ("Bad private type", Prim_Type);
                  end case;

               when A_Private_Extension_Declaration =>
                  Type_G        := Gender_Tagged;
                  Derived_Found := True;
                  exit;

               when A_Formal_Type_Declaration =>
                  case Formal_Type_Kind (Type_Declaration_View (Prim_Type)) is
                     when A_Formal_Derived_Type_Definition =>
                        Derived_Found := True;
                        if Trait_Kind (Type_Declaration_View (Prim_Type)) = A_Private_Trait then
                           Type_G := Gender_Tagged;
                           exit;
                        end if;
                     when A_Formal_Tagged_Private_Type_Definition | A_Formal_Interface_Type_Definition =>
                        Type_G := Gender_Tagged;
                     when others =>
                        null;
                  end case;

               when others =>
                  Failure ("Not a type from Primitive_Types", Prim_Type);
            end case;
         end loop;

         if not Derived_Found then
            -- not a derived operation
            return;
         end if;

         -- Determine declaration_gender and indicator_gender
         -- They have to be determined together, since bodies that are not completions are considered
         -- Gender_Declaration, but is_overriding_operation returns False for bodies not acting as spec.
         case Declaration_Kind (Callable) is
            when A_Procedure_Body_Declaration
               | A_Procedure_Body_Stub
               | A_Procedure_Renaming_Declaration
               | A_Null_Procedure_Declaration
               | A_Function_Body_Declaration
               | A_Function_Body_Stub
               | A_Function_Renaming_Declaration
               | An_Expression_Function_Declaration
               | An_Entry_Body_Declaration
               =>
               -- Note: instantiated generic bodies are not analyzed
               if Is_Nil (Corresponding_Declaration (Callable)) then
                  Declaration_G := Gender_Declaration;
                  Indicator_G   := Indicator_Table (Is_Overriding_Operation (Callable));
               else
                  Declaration_G := Gender_Body;
                  Indicator_G   := Indicator_Table (Is_Overriding_Operation (Corresponding_Declaration (Callable)));
               end if;
            when A_Procedure_Declaration
               | A_Function_Declaration
               | An_Entry_Declaration
               =>
               Declaration_G := Gender_Declaration;
               Indicator_G   := Indicator_Table (Is_Overriding_Operation (Inst_Or_Call_Decl));
            when others =>
               Failure ("Derivations.Process_Callable: bad callable", Callable);
         end case;

         case Indicator_Uses (Type_G, Indicator_G, Declaration_G) is
            when Required =>
               case Indicator_G is
                  when Gender_Overriding =>
                     if not Is_Overriding_Declaration (Callable) then
                        Report (Rule_Id,
                                Indicator_Contexts (Type_G, Indicator_G, Declaration_G),
                                Get_Location (Inst_Or_Call_Decl),
                                "Missing overriding indicator");
                        Fixes.Insert ("overriding ", Before, Inst_Or_Call_Decl);
                     end if;
                  when Gender_Not_Overriding =>
                     if not Is_Not_Overriding_Declaration (Callable) then
                        Report (Rule_Id,
                                Indicator_Contexts (Type_G, Indicator_G, Declaration_G),
                                Get_Location (Inst_Or_Call_Decl),
                                "Missing not overriding indicator");
                        Fixes.Insert ("not overriding ", Before, Inst_Or_Call_Decl);
                  end if;
               end case;
            when Forbidden =>
               case Indicator_G is
                  when Gender_Overriding =>
                     if Is_Overriding_Declaration (Callable) then
                        Report (Rule_Id,
                                Indicator_Contexts (Type_G, Indicator_G, Declaration_G),
                                Get_Location (Inst_Or_Call_Decl),
                                "Overriding indicator not allowed here");
                        Fixes.Delete (From => Get_Location (Inst_Or_Call_Decl),
                                      To   => Get_Next_Word_Location (Inst_Or_Call_Decl,
                                                                      Starting => From_Head,
                                                                      Skipping => 1));
                     end if;
                  when Gender_Not_Overriding =>
                     if Is_Not_Overriding_Declaration (Callable) then
                        Report (Rule_Id,
                                Indicator_Contexts (Type_G, Indicator_G, Declaration_G),
                                Get_Location (Inst_Or_Call_Decl),
                                "Not overriding indicator not allowed here");
                        Fixes.Delete (From => Get_Location (Callable),
                                      To   => Get_Next_Word_Location (Inst_Or_Call_Decl,
                                                                      Starting => From_Head,
                                                                      Skipping => 2));
                     end if;
               end case;
            when Unchecked =>
               null;
         end case;
      end;
   end Process_Callable;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Inst : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Iterator;
      Ignored         : Traverse_Control := Continue;
      Controlled_Inst : Null_State;

      procedure Pre_Operation  (Element       :        Asis.Element;
                                Control       : in out Traverse_Control;
                                In_Controlled : in out Null_State);

      procedure Traverse is new Traverse_Element (Null_State, Pre_Operation, Null_State_Procedure);

      procedure Pre_Operation  (Element       :        Asis.Element;
                                Control       : in out Traverse_Control;
                                In_Controlled : in out Null_State)
      is
         pragma Unreferenced (Control, In_Controlled);
      begin
         case Element_Kind (Element) is
            when A_Definition =>
               case Definition_Kind (Element) is
               when A_Type_Definition =>
                  case Type_Kind (Element) is
                     when A_Derived_Type_Definition =>
                        Check_Max_Depth (Element);
                     when A_Derived_Record_Extension_Definition
                        | An_Interface_Type_Definition
                        =>
                        Check_Max_Depth (Element);
                     when others =>
                        null;
                  end case;

               when A_Private_Extension_Definition =>
                  Check_Max_Depth (Element);

               when A_Formal_Type_Definition =>
                  case Formal_Type_Kind (Element) is
                     when A_Formal_Derived_Type_Definition
                        | A_Formal_Interface_Type_Definition
                        =>
                        Check_Max_Depth (Element);
                     when others =>
                        null;
                  end case;

               when A_Task_Definition | A_Protected_Definition =>
                  Check_Max_Depth (Element);

               when others =>
                  null;
               end case;
            when others =>
               null;
         end case;
      end Pre_Operation;
   begin -- Process_Instantiation
      if not Rule_Used (SR_Indicator) and then not Rule_Used (SR_Max_Depth) then
         return;
      end if;

      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (SR_Indicator) and Declaration_Kind (Inst) in A_Procedure_Instantiation | A_Function_Instantiation
      then
         -- A generic has always an explicit spec, and only the instantiation is available here
         -- => process the instantiated spec, ignore the body
         Process_Callable (Corresponding_Declaration (Inst));
      end if;

      if Rule_Used (SR_Max_Depth) then
         Traverse (Corresponding_Declaration (Inst), Ignored, Controlled_Inst);
         declare
            Inst_Body : constant Asis.Declaration := Corresponding_Body (Inst);
         begin
            if not Is_Nil (Inst_Body) then
               Traverse (Inst_Body, Ignored, Controlled_Inst);
            end if;
         end;
      end if;
   end Process_Instantiation;

begin  -- Rules.Derivations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Derivations;
