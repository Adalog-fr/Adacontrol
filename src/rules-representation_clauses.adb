----------------------------------------------------------------------
--  Rules.Representation_Clauses - Package body                     --
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
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Clauses,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator;

-- Adalog
with
  Binary_Map,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Queries,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.String_Set;
pragma Elaborate (Framework.Language);

package body Rules.Representation_Clauses is
   use Framework, Ada.Strings.Wide_Unbounded, Utilities;

   Storage_Unit : Thick_Queries.Biggest_Int;

   type Clause_Names is (Cl_Attribute,             Cl_At,              CL_At_Mod,
                         Cl_Enumeration,           Cl_Fractional_Size, Cl_Incomplete_Record,
                         Cl_Non_Contiguous_Record, Cl_Record);

   package Clause_Flags_Utilities is new Framework.Language.Flag_Utilities (Clause_Names, "CL_");
   use Clause_Flags_Utilities;

   -- Clause => Label
   package Context_Map is new Binary_Map (Unbounded_Wide_String, Unbounded_Wide_String);
   use Context_Map;

   Usage     : array (Rule_Types) of Context_Map.Map;

   type Usage_Flags is array (Clause_Names) of Boolean;
   Not_Used : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := Not_Used;
   Save_Used : Usage_Flags;
   Key       : array (Clause_Names range Clause_Names'Succ (Cl_Attribute) .. Clause_Names'Last)
                  of Unbounded_Wide_String;
   Key_All   : constant Unbounded_Wide_String := To_Unbounded_Wide_String ("all");

   ----------------
   -- Proper_Key --
   ----------------

   function Proper_Key (The_Key : Unbounded_Wide_String) return Wide_String is
      Img : constant Wide_String := To_Wide_String (The_Key);
   begin
      if Img (1) = ''' then
         -- attribute
         return Img;
      else
         -- Remove "Cl_"
         return To_Lower (Img (4 .. Img'Last));
      end if;
   end Proper_Key;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message  ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter(s):", Footer => "(optional)", Extra_Value => "<specifiable attribute>");
      User_Message  ("Control occurrences of representation clauses");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language;
      Clause : Clause_Names;
      Param  : Unbounded_Wide_String;

   begin
      if Parameter_Exists then
         if Is_Present (Usage (Rule_Type), Key_All) then
            Parameter_Error (Rule_Id, "rule already specified for all representation clauses");
         end if;
      else
         if not Is_Empty (Usage (Rule_Type)) then
            Parameter_Error (Rule_Id, "some representation clauses already specified");
         end if;
         Add (Usage (Rule_Type), Key_All, To_Unbounded_Wide_String (Label));
         Rule_Used := (others => True);
      end if;

      while Parameter_Exists loop
         Clause := Get_Flag_Parameter (Allow_Any => True);
         if Clause = Cl_Attribute then
            Param := To_Unbounded_Wide_String (Get_Name_Parameter);
            if Element (Param, 1) /= ''' then
               Parameter_Error (Rule_Id, "parameter must be at, at_mod, enumeration, record, or an attribute");
            end if;
         else
            Param := To_Unbounded_Wide_String (Clause_Names'Wide_Image (Clause));
         end if;

         if Is_Present (Usage (Rule_Type), Param) then
            Parameter_Error (Rule_Id, "clause already given: " & Proper_Key (Param));
         end if;

         Add (Usage (Rule_Type), Param, To_Unbounded_Wide_String (Label));
         Rule_Used (Clause) := True;
      end loop;

   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            for T in Usage'Range loop
               Clear (Usage (T));
            end loop;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := Not_Used;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      use Asis.Declarations;
      use Framework.Queries, Thick_Queries;
   begin
      if Rule_Used = Not_Used then
         return;
      end if;

      Storage_Unit := Biggest_Int'Wide_Value (Static_Expression_Value_Image
                                              (Initialization_Expression
                                               (System_Value ("STORAGE_UNIT"))));
   end Prepare;


   --------------------
   -- Process_Clause --
   --------------------

   procedure Process_Clause (Rep_Clause : in Asis.Representation_Clause) is
      use Asis, Asis.Clauses, Asis.Elements, Asis.Expressions, Thick_Queries;
      use Framework.Reports;

      Attribute : Unbounded_Wide_String;

      procedure Check_Usage (Clause  : Clause_Names;
                             Message : Wide_String;
                             Loc     : Location := Get_Location (Rep_Clause))
      is
         Key_Map : Unbounded_Wide_String;
      begin
         if Clause = Cl_Attribute then
            Key_Map := Attribute;
         else
            Key_Map := Key (Clause);
         end if;

         -- Report the highest priority from Check/Search
         if Is_Present (Usage (Check), Key_Map) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Check), Key_Map)),
                    Check,
                    Loc,
                    Message);
         elsif Is_Present (Usage (Check), Key_All) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Check), Key_All)),
                    Check,
                    Loc,
                    Message);
         elsif Is_Present (Usage (Search), Key_Map) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Search), Key_Map)),
                    Search,
                    Loc,
                    Message);
         elsif Is_Present (Usage (Search), Key_All) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Search), Key_All)),
                    Search,
                    Loc,
                    Message);
         end if;

         -- Always report Count
         if Is_Present (Usage (Count), Key_Map) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Count), Key_Map)),
                    Count,
                    Loc,
                    Message);
         elsif Is_Present (Usage (Count), Key_All) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Count), Key_All)),
                    Count,
                    Loc,
                    Message);
         end if;
      end Check_Usage;

      procedure Check_Incomplete (Clause : Asis.Representation_Clause) is
         use Asis.Declarations;
         use Framework.String_Set;

         Components : constant Asis.Component_Clause_List := Component_Clauses (Clause);
         Compo_Set  : Set;

         procedure Pre_Procedure (Element : in     Asis.Element;
                                  Control : in out Asis.Traverse_Control;
                                  State   : in out Null_State)
         is
            pragma Unreferenced (Control, State);
         begin
            if Element_Kind (Element) = A_Defining_Name then
               if not Is_Present (Compo_Set, To_Upper (Defining_Name_Image (Element))) then
                  Check_Usage (Cl_Incomplete_Record,
                               "no component clause for "
                               & Defining_Name_Image (Element)
                               & " at "
                               & Image (Get_Location (Element)));
               end if;
            end if;
         end Pre_Procedure;

         procedure Traverse_Type is new Asis.Iterator.Traverse_Element (Null_State,
                                                                        Pre_Procedure,
                                                                        Null_State_Procedure);
         Control : Asis.Traverse_Control := Continue;
         State   : Null_State;
         Decl    : constant Asis.Declaration
           := Corresponding_Name_Declaration (Representation_Clause_Name (Clause));
      begin
         for C in Components'Range loop
            Add (Compo_Set, To_Upper (Name_Image (Representation_Clause_Name (Components (C)))));
         end loop;

         if not Is_Nil (Discriminant_Part (Decl)) then
            Traverse_Type (Discriminant_Part (Decl), Control, State);
         end if;

         Traverse_Type (Type_Declaration_View (Decl), Control, State);

         Clear (Compo_Set);
      end Check_Incomplete;

      procedure Check_Contiguous (Clause : Asis.Representation_Clause) is
         use Asis.Definitions;

         type Field_Descriptor is
            record
               Low, High : Biggest_Int;
               Compo_Inx : Asis.List_Index;
            end record;

         Components    : constant Asis.Component_Clause_List := Component_Clauses (Clause);
         Fields        : array (Components'Range) of Field_Descriptor;
         Used_F        : ASIS_Natural := Fields'First - 1;
         Size_Expr     : Asis.Expression;
         Is_Uncheck    : Boolean := False;
         Starting_Unit : Biggest_Int;
      begin
         for C in Components'Range loop
            declare
               Pos : constant Wide_String := Static_Expression_Value_Image (Component_Clause_Position (Components (C)));
               P   : Biggest_Int;
               R   : constant Asis.Discrete_Range := Component_Clause_Range (Components (C));
               L   : constant Wide_String := Static_Expression_Value_Image (Lower_Bound (R));
               H   : constant Wide_String := Static_Expression_Value_Image (Upper_Bound (R));
               F   : Field_Descriptor;
               Ins : Asis.List_Index;
            begin
               if Pos = "" or L = "" or H = "" then
                  Uncheckable (Rule_Id,
                               False_Negative,
                               Get_Location (Components (C)),
                               "unable to evaluate component position for non_contiguous_record subrule");
                  Is_Uncheck := True;
               else
                  P := Biggest_Int'Wide_Value (Pos) * Storage_Unit;
                  F := (Low       => P + Biggest_Int'Wide_Value (L),
                        High      => P + Biggest_Int'Wide_Value (H),
                        Compo_Inx => C);

                  -- Insert F at the right place
                  -- Since it is highly likely that clauses are given in order, start
                  -- from the end.
                  Ins := Fields'First;
                  for I in reverse List_Index range Fields'First .. Used_F loop
                     pragma Warnings (Off);
                     -- Gnat warns that "Fields" may be referenced before it has a value
                     -- but this cannot happen since the loop is bounded by Used_F
                     if Fields (I).Low < F.Low then
                        Ins := I + 1;
                        exit;
                     end if;
                     pragma Warnings (On);
                     Fields (I + 1) := Fields (I);
                  end loop;
                  Fields (Ins) := F;
                  if Ins /= Fields'First
                    and then Fields (Ins - 1).High > Fields (Ins).High
                  then
                     Fields (Ins).High := Fields (Ins - 1).High;
                  end if;
                  Used_F := Used_F + 1;
               end if;
            end;
         end loop;
         if Is_Uncheck then
            return;
         end if;

         if Fields (Fields'First).Low /= 0 then
            Check_Usage (Cl_Non_Contiguous_Record,
                         "gap at 0 range 0.." & Biggest_Int_Img(Fields (Fields'First).Low-1),
                         Get_Location (Components (Fields(Fields'First).Compo_Inx)));
         end if;
         for I in List_Index range Fields'First+1 .. Fields'Last loop
            if Fields (I - 1).High + 1 < Fields (I).Low then
               Starting_Unit := (Fields (I - 1).High + 1) / Storage_Unit;
               Check_Usage (Cl_Non_Contiguous_Record,
                            "gap before component, at "
                            & Biggest_Int_Img (Starting_Unit)
                            & " range "
                            & Biggest_Int_Img (Fields (I - 1).High + 1 - Starting_Unit * Storage_Unit)
                            & ".."
                            & Biggest_Int_Img (Fields (I).Low - 1 - Starting_Unit * Storage_Unit),
                            Get_Location (Components (Fields (I).Compo_Inx)));
            end if;
         end loop;

         -- Check gap at the end if size clause given
         Size_Expr := Size_Clause_Expression (Representation_Clause_Name (Clause));
         if Is_Nil (Size_Expr) then
            return;
         end if;
         declare
            S : constant Wide_String := Static_Expression_Value_Image (Size_Expr);
         begin
            if S = "" then
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Clause),
                            "unable to evaluate size of "
                            & Name_Image (Representation_Clause_Name (Clause))
                            & "for non_contiguous_record subrule");
               return;
            end if;

            if Fields (Fields'Last).High /= Biggest_Int'Wide_Value (S) - 1 then
               Starting_Unit := (Fields (Fields'Last).High + 1) / Storage_Unit;
               Check_Usage (Cl_Non_Contiguous_Record,
                            "gap at end of record, at "
                            & Biggest_Int_Img (Starting_Unit)
                            & " range "
                            & Biggest_Int_Img (Fields (Fields'Last).High + 1 - Starting_Unit * Storage_Unit)
                            & ".."
                            & Biggest_Int_Img (Biggest_Int'Wide_Value (S) - 1 - Starting_Unit * Storage_Unit),
                            Get_Location (Components (Fields (Fields'Last).Compo_Inx)));
            end if;
         end;
      end Check_Contiguous;

   begin   -- Process_Clause
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Representation_Clause_Kind (Rep_Clause) is
         when Not_A_Representation_Clause =>
            Failure ("Not a representation clause in " & Rule_Id);

         when An_Attribute_Definition_Clause =>
            if not Rule_Used (Cl_Attribute) and not Rule_Used (Cl_Fractional_Size) then
               return;
            end if;

            Attribute := To_Unbounded_Wide_String (''' & To_Upper (Attribute_Name_Image
                                                                   (Representation_Clause_Name (Rep_Clause))));
            if Expression_Kind (Prefix (Representation_Clause_Name (Rep_Clause))) = An_Attribute_Reference then
               -- This happens only in for T'Class'Read and similar

               -- ASIS BUG:
               -- Attribute_Designator_Identifier returns wrong value on double attributes.
               -- (Attribute_Designator_Identifer is used by Attribute_Name_Image)

               -- The following case statement should be:
               --    Attribute := "'CLASS" & Attribute;
               -- Unfortunately, Attribute is wrong, hence the following kludge.
               -- Note that we call Asis.Elements.Attribute_Kind, not A4G_Bugs.Attribute_Kind
               -- because the version in A4G_Bugs does not work due to the same ASIS bug, but the
               -- regular version seems to work in this case. Sigh.
               case Attribute_Kind (Representation_Clause_Name (Rep_Clause)) is --## RULE LINE OFF Use_A4G_Bugs
                  when A_Read_Attribute =>
                     Attribute := To_Unbounded_Wide_String ("'CLASS'READ");
                  when A_Write_Attribute =>
                     Attribute := To_Unbounded_Wide_String ("'CLASS'WRITE");
                  when An_Input_Attribute =>
                     Attribute := To_Unbounded_Wide_String ("'CLASS'INPUT");
                  when An_Output_Attribute =>
                     Attribute := To_Unbounded_Wide_String ("'CLASS'OUTPUT");
                  when others =>
                     Failure ("Unexpected double attribute in " & Rule_Id, Rep_Clause);
               end case;
            end if;

            Check_Usage (Cl_Attribute, "use of representation clause for " & To_Wide_String (Attribute));

            if Attribute = "'SIZE" and then Rule_Used (Cl_Fractional_Size) then
               declare
                  Value_Str : constant Wide_String
                    := Static_Expression_Value_Image (Representation_Clause_Expression (Rep_Clause));
               begin
                  if Value_Str = "" then
                     Uncheckable (Rule_Id,
                                  False_Negative,
                                  Get_Location (Representation_Clause_Expression (Rep_Clause)),
                                  "unable to evaluate size for fractional_size subrule");
                  elsif Biggest_Int'Wide_Value (Value_Str) rem Storage_Unit /= 0 then
                     Check_Usage (Cl_Fractional_Size, "size clause not multiple of Storage_Unit");
                  end if;
               end;
            end if;

         when An_Enumeration_Representation_Clause =>
            Check_Usage (Cl_Enumeration, "use of enumeration representation clause");

         when A_Record_Representation_Clause =>
            Check_Usage (Cl_Record, "use of record representation clause");

            if Rule_Used (Cl_At_Mod) and then not Is_Nil (Mod_Clause_Expression (Rep_Clause)) then
               Check_Usage (CL_At_Mod, "use of Ada 83 alignment clause");
            end if;

            if Rule_Used (Cl_Incomplete_Record) then
               Check_Incomplete (Rep_Clause);
            end if;

            if Rule_Used (Cl_Non_Contiguous_Record) then
               Check_Contiguous (Rep_Clause);
            end if;

         when An_At_Clause =>
            Check_Usage (Cl_At, "use of Ada 83 address clause");
      end case;
   end Process_Clause;

begin
   for K in Clause_Names range Clause_Names'Succ (Cl_Attribute) .. Clause_Names'Last loop
      Key (K) := To_Unbounded_Wide_String (Clause_Names'Wide_Image (K));
   end loop;

   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB    => Help'Access,
                                     Add_Use_CB => Add_Use'Access,
                                     Command_CB => Command'Access,
                                     Prepare_CB => Prepare'Access);
end Rules.Representation_Clauses;
