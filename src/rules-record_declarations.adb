----------------------------------------------------------------------
--  Rules.Record_Declarations - Package body                        --
--                                                                  --
--  This software is (c) ANSALDO and Adalog 2004-2010.              --
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

-- ASIS
with
  Asis.Clauses,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  A4G_Bugs,
  Thick_Queries,
  Utilities;

-- AdaControl
with
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Queries;
pragma Elaborate (Framework.Language);
pragma Elaborate (Framework.Language.Shared_Keys);

package body Rules.Record_Declarations is
   use Framework, Framework.Control_Manager, Framework.Language.Shared_Keys;

   Storage_Unit : Thick_Queries.Biggest_Int;

   type Subrules is (Component); -- More subrules expected in the future
   pragma Warnings (Off);  -- Gnat warns about possible CE when instantiating with a single value
   package Subrules_Flag_Utilities  is new Framework.Language.Flag_Utilities (Subrules);
   pragma Warnings (On);

   type Usage is array (Subrules) of Control_Kinds_Set;
   Not_Used  : constant Usage := (others => Empty_Control_Kinds_Set);
   Rule_Used : Usage := Not_Used;
   Save_Used : Usage;

   type Repr_Condition is (None, Present, Absent);
   type Compo_Context is new Basic_Rule_Context with
      record
         In_Variant  : Repr_Condition;
         Packing     : Repr_Condition;
         Sizing      : Repr_Condition;
         Initialized : Repr_Condition;
         Aligned     : Repr_Condition;
      end record;
   Compo_Contexts : Context_Store;
   package Compo_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Compo_Contexts);

   Expected_Categories : constant Categories_Set := Basic_Set + Cat_Private;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls various parameters related to record types declarations");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags (Header => "Parameter(1):");
      User_Message;
      User_Message ("For component:");
      User_Message ("Parameter(2): <entity>|<category>");
      Help_On_Categories (Expected => Expected_Categories);
      User_Message ("Parameter(3..): [not] in_variant | packed | sized | initialized | aligned (optional)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Subrules_Flag_Utilities;
      Subrule : Subrules;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "two or three parameters required");
      end if;

      case Subrule is
         when Component =>
            declare
               Entity  : constant Entity_Specification := Get_Entity_Parameter (Allow_Extended => Parens_OK);

               In_Variant  : Repr_Condition := None;
               Packing     : Repr_Condition := None;
               Sizing      : Repr_Condition := None;
               Initialized : Repr_Condition := None;
               Aligned     : Repr_Condition := None;
               Temp        : Repr_Condition;
            begin
               Check_Category (Rule_Id, Entity, Expected_Categories);

               while Parameter_Exists loop
                  if Get_Modifier ("NOT") then
                     Temp := Absent;
                  else
                     Temp := Present;
                  end if;

                  declare
                     Name : constant Wide_String := Get_Name_Parameter;
                  begin
                     if Name = "IN_VARIANT" then
                        if In_Variant /= None then
                           Parameter_Error (Rule_Id, "in_variant already specified");
                        end if;
                        In_Variant := Temp;
                     elsif Name = "PACKED" then
                        if Packing /= None then
                           Parameter_Error (Rule_Id, "packing already specified");
                        end if;
                        Packing := Temp;
                     elsif Name = "SIZED" then
                        if Sizing /= None then
                           Parameter_Error (Rule_Id, "sizing already specified");
                        end if;
                        Sizing := Temp;
                     elsif Name = "INITIALIZED" then
                        if Initialized /= None then
                           Parameter_Error (Rule_Id, "initialization already specified");
                        end if;
                        Initialized := Temp;
                     elsif Name = "ALIGNED" then
                        if Aligned /= None then
                           Parameter_Error (Rule_Id, "alignment already specified");
                        end if;
                        Aligned := Temp;
                     else
                        Parameter_Error (Rule_Id, """in_variant"", ""packed"", ""sized"", or ""initialized"" expected");
                     end if;
                  end;
               end loop;
               Associate (Compo_Contexts,
                          Entity,
                          Compo_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label)
                             with In_Variant, Packing, Sizing, Initialized, Aligned),
                          Additive => True);
            exception
               when Already_In_Store =>
                  Parameter_Error (Rule_Id, "entity already given: " & Image (Entity));
            end;
            Rule_Used (Subrule) := (others => True);
      end case;

    end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := Not_Used;
            Clear (Compo_Contexts);
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
      use Framework.Queries;
   begin
      if Rule_Used = Not_Used then
         return;
      end if;

      Storage_Unit := System_Value ("STORAGE_UNIT");
   end Prepare;


   -------------------------------
   -- Process_Record_Definition --
   -------------------------------

   procedure Process_Record_Definition (Definition : Asis.Definition) is

      procedure Process_Component (Compo : Asis.Expression; In_Variant : Boolean) is
         use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements;
         use Thick_Queries, Utilities;

         Compo_Type : Asis.Declaration;
         First_St   : Asis.Declaration;
         Iterator   : Context_Iterator := Compo_Iterator.Create;

         Is_Packed      : Boolean;
         Is_Initialized : Boolean;

         procedure Compo_Report (Iter : in out Context_Iterator; In_Case : Casing) is
            use Ada.Strings.Wide_Unbounded;
            use Asis.Clauses;
            use Framework.Locations, Framework.Reports;
            Extra1          : Unbounded_Wide_String;
            Extra2          : Unbounded_Wide_String;
            Current_Context : Compo_Context;
            Is_Sized        : Boolean;
            Is_Aligned      : Boolean;
            Applicable1     : Boolean;
            Applicable2     : Boolean;
         begin
            while not Is_Exhausted (Iter) loop
               Current_Context := Compo_Context (Value (Iter));
               Applicable1     := True;
               Extra1          := Null_Unbounded_Wide_String;

               case Current_Context.In_Variant is
                  when None =>
                     null;
                  when Present =>
                     if In_Variant then
                        Append (Extra1, "in variant ");
                     else
                        Applicable1 := False;
                     end if;
                  when Absent =>
                     if In_Variant then
                        Applicable1 := False;
                     else
                        Append (Extra1, "not in variant ");
                     end if;
               end case;

               case Current_Context.Packing is
                  when None =>
                     null;
                  when Present =>
                     if Is_Packed then
                        Append (Extra1, "packed ");
                     else
                        Applicable1 := False;
                     end if;
                  when Absent =>
                     if Is_Packed then
                        Applicable1 := False;
                     else
                        Append (Extra1, "unpacked ");
                     end if;
               end case;

               case Current_Context.Initialized is
                  when None =>
                     null;
                  when Present =>
                     if Is_Initialized then
                        Append (Extra1, "initialized ");
                     else
                        Applicable1 := False;
                     end if;
                  when Absent =>
                     if Is_Initialized then
                        Applicable1 := False;
                     else
                        Append (Extra1, "uninitialized ");
                     end if;
               end case;

               declare
                  Compo_Clause : Asis.Component_Clause;
                  L            : Extended_Biggest_Natural;
               begin
                  for Compo_Name : Asis.Name of Names (Compo) loop
                     Extra2      := Null_Unbounded_Wide_String;
                     Applicable2 := True;
                     Compo_Clause := Corresponding_Component_Clause (Compo_Name);
                     if Is_Nil (Compo_Clause) then
                        Is_Sized    := False;
                        Is_Aligned  := True;
                     else
                        Is_Sized   := True;
                        L  := Discrete_Static_Expression_Value (Lower_Bound
                                                                (Component_Clause_Range (Compo_Clause)));
                        if L = Not_Static then
                           -- We know damn well it IS static, but there are a few cases that we can't evaluate...
                           case Current_Context.Aligned is
                              when None =>
                                 null;   -- Don't care, not checked
                              when Present =>
                                 Uncheckable (Rule_Id,
                                              False_Positive,
                                              Get_Location (Compo_Clause),
                                              "unable to evaluate component position, assuming aligned");
                                 Is_Aligned := True;
                              when Absent =>
                                 Uncheckable (Rule_Id,
                                              False_Positive,
                                              Get_Location (Compo_Clause),
                                              "unable to evaluate component position, assuming not aligned");
                                 Is_Aligned := False;
                           end case;
                        else
                           Is_Aligned := L rem Storage_Unit = 0;
                        end if;
                     end if;

                     case Current_Context.Sizing is
                        when None =>
                           null;
                        when Present =>
                           if Is_Sized then
                              Append (Extra2, "sized ");
                           else
                              Applicable2 := False;
                           end if;
                        when Absent =>
                           if Is_Sized then
                              Applicable2 := False;
                           else
                              Append (Extra2, "unsized ");
                           end if;
                     end case;

                     case Current_Context.Aligned is
                        when None =>
                           null;
                        when Present =>
                           if Is_Aligned then
                              Append (Extra2, "aligned ");
                           else
                              Applicable2 := False;
                           end if;
                        when Absent =>
                           if Is_Aligned then
                              Applicable2 := False;
                           else
                              Append (Extra2, "unaligned ");
                           end if;
                     end case;

                     if Applicable1 and Applicable2 then
                        Report (Rule_Id,
                                Current_Context,
                                Get_Location (Compo_Name),
                                "Component " & Defining_Name_Image (Compo_Name)
                                & " is "
                                & To_Wide_String (Extra1)
                                & To_Wide_String (Extra2)
                                & '"' & Set_Casing (Last_Matching_Name (Compo_Contexts), In_Case) & '"');
                     end if;
                  end loop;
               end;
               Next (Iter);
            end loop;
         end Compo_Report;

         use Asis.Expressions;
         Temp : Asis.Element;
      begin  -- Process_Component

         -- Get rid of special cases (null components, variants)
         case Element_Kind (Compo) is
            when A_Declaration =>
               -- Note: all other branches of this case statement end with return or Failure
               if Declaration_Kind (Compo) /= A_Component_Declaration then
                  Failure ("Unexpected declaration for component", Compo);
               end if;
            when A_Definition =>
               case Definition_Kind (Compo) is
                  when A_Null_Component =>
                     return;
                  when A_Variant_Part =>
                     for V : Asis.Variant of Variants (Compo) loop
                        for C : Asis.Record_Component of Record_Components (V) loop
                           Process_Component (C, In_Variant => True);
                        end loop;
                     end loop;
                     return;
                  when others =>
                     Failure ("Unexpected definition for component", Compo);
               end case;
            when others =>
               Failure ("Unexpected element for component", Compo);
         end case;

         -- Here we are in the case of a good ol' regular component
         Temp := Component_Definition_View (Object_Declaration_View (Compo));
         if Definition_Kind (Temp) = An_Access_Definition then
            -- Component is of an anonymous access type
            -- Cannot be packed
            -- Might be initialized
            -- Cannot correspond to a named type component
            -- Can only match category An_Access_Type
            Is_Packed := False;
            Is_Initialized := not Is_Nil (Initialization_Expression (Compo));

            Reset (Iterator, Framework.Value ("ACCESS"));
            Compo_Report (Iterator, Lower_Case);
            return;
         end if;

         -- Relief! The component has an explicit type
         -- The only possible attribute is 'Base, we can ignore it (no 'Class for components)
         Compo_Type := Corresponding_Name_Declaration (Strip_Attributes (Subtype_Simple_Name (Temp)));
         Is_Packed  := (for some P of Corresponding_Pragmas (Compo_Type) => Pragma_Kind (P) = A_Pack_Pragma);

         -- Kludge for ASIS bug [J226-011], fixed in GPL2010 and Pro-6.4.1
         -- Strings declared in STANDARD have no pragma Pack
         if not Is_Packed then
            declare
               Type_Name : constant Wide_String := To_Upper
                                                   (Full_Name_Image
                                                    (Names (Ultimate_Type_Declaration (Compo_Type,
                                                                                       Privacy => Follow_Private))
                                                           (1)));
            begin
               Is_Packed :=         Type_Name = "STANDARD.STRING"
                            or else Type_Name = "STANDARD.WIDE_STRING";
               --2005 add Wide_Wide_String (if bug not fixed)
            end;
         end if;

         Is_Initialized := not Is_Nil (Initialization_Expression (Compo));

         -- Exact subtype
         Reset (Iterator, Names (Compo_Type)(1), Extend_To => All_Extensions);
         Compo_Report (Iterator, Title_Case);

         -- First subtype (aka type), if different
         First_St := A4G_Bugs.Corresponding_First_Subtype (Compo_Type);
         if not Is_Equal (First_St, Compo_Type) then
            Reset (Iterator, Names (First_St) (1), Extend_To => All_Extensions);
            Compo_Report (Iterator, Title_Case);
         end if;

         -- Category
         Reset (Iterator, Framework.Value (Image (Type_Category (First_St, Follow_Derived => True))));
         Compo_Report (Iterator, Lower_Case);
      end Process_Component;

      use Asis.Definitions;
   begin  -- Process_Record_Definition
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if not Is_Empty (Compo_Contexts) then
         for C : Asis.Record_Component of Record_Components (Definition) loop
            Process_Component (C, In_Variant => False);
         end loop;
      end if;
   end Process_Record_Definition;

begin  -- Rules.Record_Declarations
   Rules_Manager.Register (Rule_Id,
                           Rules_Manager.Semantic,
                           Help_CB        => Help'Access,
                           Add_Control_CB => Add_Control'Access,
                           Command_CB     => Command'Access,
                           Prepare_CB     => Prepare'Access);
end Rules.Record_Declarations;
