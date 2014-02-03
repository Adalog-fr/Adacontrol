----------------------------------------------------------------------
--  Rules.Representation_Clauses - Package body                     --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2012. The Ada --
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
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Queries,
  Framework.String_Set;
pragma Elaborate (Framework.Language);

package body Rules.Representation_Clauses is
   use Framework, Utilities;

   Storage_Unit : Thick_Queries.Biggest_Int;

   -- Sr_Attribute must stay first
   type Subrules is (Sr_Attribute,           Sr_At,                    Sr_At_Mod,
                     Sr_Enumeration,         Sr_Fractional_Size,       Sr_Incomplete_Layout,
                     Sr_Layout,              Sr_Non_Aligned_Component, Sr_Non_Contiguous_Layout,
                     Sr_No_Bit_Order_Layout, Sr_Overlay);

   package Subrules_Flags_Utilities is new Framework.Language.Flag_Utilities (Subrules, "SR_");
   use Subrules_Flags_Utilities, Framework.Language.Shared_Keys.Categories_Utilities;

   -- Subrule => Context
   type Repr_Context is new Framework.Control_Manager.Basic_Rule_Context with
      record
         Cats        : Framework.Language.Shared_Keys.Categories_Utilities.Modifier_Set;
         Global_Only : Boolean;
         Obj_Only    : Boolean;
      end record;
   Usage : Framework.Control_Manager.Context_Store;
   package Repr_Context_Init is new Framework.Control_Manager.Generic_Context_Iterator (Usage);

   type Usage_Flags is array (Subrules) of Boolean;
   Not_Used  : constant Usage_Flags := (others => False);
   Rule_Used : Usage_Flags := Not_Used;
   Save_Used : Usage_Flags;
   Key       : array (Subrules range Subrules'Succ (Sr_Attribute) .. Subrules'Last) of Entity_Specification;
   Key_All   : constant Entity_Specification := Value ("all");

   ----------------
   -- Proper_Key --
   ----------------

   function Proper_Key (The_Key : Entity_Specification) return Wide_String is
      Img : constant Wide_String := Image (The_Key);
   begin
      if Img (1) = ''' then
         -- attribute
         return Img;
      else
         -- Remove "Sr_"
         return To_Lower (Img (4 .. Img'Last));
      end if;
   end Proper_Key;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("Control occurrences of representation clauses");
      User_Message;
      Help_On_Flags (Header      => "Parameter(s): [<categories>]",
                     Footer      => "(optional)",
                     Extra_Value => "[global] [object] <specifiable attribute>");
      Help_On_Modifiers (Header => "Categories:");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   Expected_Categories : constant Modifier_Set := (Framework.Language.Shared_Keys.Cat_Any => False, others => True);

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Control_Manager, Framework.Language;
      Subrule   : Subrules;
      Param     : Entity_Specification;
      Cats      : Modifier_Set;
      Glob_Only : Boolean := False;
      Obj_Only  : Boolean := False;
   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Cats := Get_Modifier_Set (Expected => Expected_Categories);

            Subrule := Get_Flag_Parameter (Allow_Any => True);
            if Subrule = Sr_Attribute then
               Glob_Only := Get_Modifier ("GLOBAL");
               Obj_Only  := Get_Modifier ("OBJECT");
               Param     := Value (Get_Name_Parameter);
               if Image (Param) (1) /= ''' then
                  Parameter_Error (Rule_Id, "parameter must be an attribute");
               end if;
            else
               Param := Value (Subrules'Wide_Image (Subrule));
            end if;

            begin
               Associate (Usage,
                          Param,
                          Repr_Context' (Basic.New_Context (Ctl_Kind, Ctl_Label) with Cats, Glob_Only, Obj_Only),
                          Additive => True);
            exception
               when Already_In_Store =>
                  Parameter_Error (Rule_Id, "clause already given: " & Proper_Key (Param));
            end;
            Rule_Used (Subrule) := True;
         end loop;

      else
         Associate (Usage, Key_All, Repr_Context'(Basic.New_Context (Ctl_Kind, Ctl_Label)
                                    with Full_Set, Global_Only => False, Obj_Only => False));
         Rule_Used := (others => True);
      end if;

   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Control_Manager, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := Not_Used;
            Clear (Usage);
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


   --------------------
   -- Process_Clause --
   --------------------

   procedure Process_Clause (Rep_Clause : in Asis.Representation_Clause) is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Clauses, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Thick_Queries;

      Attribute : Unbounded_Wide_String;
      Pfx       : Asis.Expression;

      procedure Do_Report (Subrule  : Subrules;
                           Target   : Asis.Clause;
                           Message  : Wide_String;
                           Loc      : Location := Get_Location (Rep_Clause))
      is
         use Framework.Control_Manager, Framework.Language.Shared_Keys;
         Key_Map   : Entity_Specification;
         Iter      : Context_Iterator := Repr_Context_Init.Create;
         Cont      : Repr_Context;
         Cat       : Categories;
         Pfx_Decl  : Asis.Declaration;
         Is_Object : Boolean;
         Is_Global : Boolean;

         function Clause_Name_Declaration return Asis.Declaration is
            Name : constant Asis.Expression := Representation_Clause_Name (Target);
         begin
            if Expression_Kind (Name) = An_Attribute_Reference then
               return Corresponding_Name_Declaration (Prefix (Name));
            else
               return Corresponding_Name_Declaration (Name);
            end if;
         end Clause_Name_Declaration;

      begin  -- Do_Report
         if Subrule = Sr_Attribute then
            Key_Map := Value (To_Wide_String (Attribute));
         else
            Key_Map := Key (Subrule);
         end if;

         if Subrule = Sr_Attribute then
            Pfx := Prefix (Representation_Clause_Name (Rep_Clause));
            if Expression_Kind (Pfx) = An_Attribute_Reference then
               -- T'Class'Read, certainly not an object
               Is_Object := False;

               -- Scope is the same as the ultimate prefix
               while Expression_Kind (Pfx) = An_Attribute_Reference loop
                  Pfx := Prefix (Pfx);
               end loop;
               Is_Global := Static_Level (Corresponding_Name_Declaration (Simple_Name (Pfx))) = Global_Level;
            else
               Pfx_Decl  := Corresponding_Name_Declaration (Pfx);
               Is_Object := Declaration_Kind (Pfx_Decl) in An_Object_Declaration;
               Is_Global := Static_Level (Pfx_Decl) = Global_Level;
            end if;
         else
            Is_Object := False;
            Is_Global := False;
         end if;

         Reset (Iter, Key_Map);
         while not Is_Exhausted (Iter) loop
            Cont := Repr_Context (Value (Iter));
            if    (Is_Object or not Cont.Obj_Only)
              and (Is_Global or not Cont.Global_Only)
            then
               if Cont.Cats = Empty_Set then
                  Report (Rule_Id, Cont, Loc,   Choose (Is_Global and Cont.Global_Only, "global ", "")
                                              & Choose (Is_Object and Cont.Obj_Only,    "object ", "")
                                              & Message);
               else
                  Cat := Matching_Category (Clause_Name_Declaration, Cont.Cats, Separate_Extension => True);
                  if Cat /= Cat_Any then
                     Report (Rule_Id, Cont, Loc,   Choose (Is_Global and Cont.Global_Only, "global ", "")
                                                 & Choose (Is_Object and Cont.Obj_Only,    "object ", "")
                                                 & Message & " for " & Image (Cat));
                  end if;
               end if;
            end if;
            Next (Iter);
         end loop;

         Reset (Iter, Key_All);
         while not Is_Exhausted (Iter) loop
            -- No categories for All
            Report (Rule_Id, Repr_Context (Value (Iter)), Loc, Message);
            Next (Iter);
         end loop;

      end Do_Report;

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
                  Do_Report (Sr_Incomplete_Layout,
                             Rep_Clause,
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
         Decl    : constant Asis.Declaration := Corresponding_Name_Declaration (Representation_Clause_Name (Clause));
      begin  -- Check_Incomplete
         for C in Components'Range loop
            Add (Compo_Set, To_Upper (Name_Image (Representation_Clause_Name (Components (C)))));
         end loop;

         if not Is_Nil (Discriminant_Part (Decl)) then
            Traverse_Type (Discriminant_Part (Decl), Control, State);
         end if;

         Traverse_Type (Type_Declaration_View (Decl), Control, State);

         Clear (Compo_Set);
      end Check_Incomplete;

      procedure Check_Components (Clause : Asis.Representation_Clause) is
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
               Pos : constant Extended_Biggest_Natural
                 := Discrete_Static_Expression_Value (Component_Clause_Position (Components (C)));
               P   : Biggest_Int;
               R   : constant Asis.Discrete_Range      := Component_Clause_Range (Components (C));
               L   : constant Extended_Biggest_Natural := Discrete_Static_Expression_Value (Lower_Bound (R));
               H   : constant Extended_Biggest_Int     := Discrete_Static_Expression_Value (Upper_Bound (R));
               -- H can be < 0 for empty fields
               F   : Field_Descriptor;
               Ins : Asis.List_Index;
            begin
               if Pos = Not_Static or L = Not_Static or H = Not_Static then
                  Uncheckable (Rule_Id,
                               False_Negative,
                               Get_Location (Components (C)),
                               "unable to evaluate component position for "
                               & "non_contiguous_layout and non_aligned_component subrules");
                  Is_Uncheck := True;
               else
                  P := Pos * Storage_Unit;
                  F := (Low       => P + L,
                        High      => P + H,
                        Compo_Inx => C);

                  -- Insert F at the right place
                  -- Since it is highly likely that clauses are given in order, start
                  -- from the end.
                  Ins := Fields'First;
                  for I in reverse List_Index range Fields'First .. Used_F loop
                     -- Gnat warns that "Fields" may be referenced before it has a value
                     -- but this cannot happen since the loop is bounded by Used_F
                     pragma Warnings (Off);
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
            Do_Report (Sr_Non_Contiguous_Layout,
                       Rep_Clause,
                       "gap at 0 range 0.." & Biggest_Int_Img (Fields (Fields'First).Low - 1),
                       Get_Location (Components (Fields (Fields'First).Compo_Inx)));
         end if;
         if Fields (Fields'First).Low rem Storage_Unit /= 0 then
            Do_Report (Sr_Non_Aligned_Component,
                       Components (Fields (Fields'First).Compo_Inx),
                       "unaligned component starts at bit "
                         & Biggest_Int_Img (Fields (Fields'First).Low rem Storage_Unit),
                       Get_Location (Components (Fields (Fields'First).Compo_Inx)));
         end if;
         for I in List_Index range Fields'First+1 .. Fields'Last loop
            if Fields (I - 1).High + 1 < Fields (I).Low then
               Starting_Unit := (Fields (I - 1).High + 1) / Storage_Unit;
               Do_Report (Sr_Non_Contiguous_Layout,
                          Rep_Clause,
                          "gap before component, at "
                          & Biggest_Int_Img (Starting_Unit)
                          & " range "
                          & Biggest_Int_Img (Fields (I - 1).High + 1 - Starting_Unit * Storage_Unit)
                          & ".."
                          & Biggest_Int_Img (Fields (I).Low - 1 - Starting_Unit * Storage_Unit),
                          Get_Location (Components (Fields (I).Compo_Inx)));
            end if;
            if Fields (I).Low rem Storage_Unit /= 0 then
              Do_Report (Sr_Non_Aligned_Component,
                         Components (Fields (I).Compo_Inx),
                         "unaligned component starts at bit "
                          & Biggest_Int_Img (Fields (I).Low rem Storage_Unit),
                         Get_Location (Components (Fields (I).Compo_Inx)));
            end if;
         end loop;

         -- Check gap at the end if size clause given
         Size_Expr := Attribute_Clause_Expression (A_Size_Attribute, Representation_Clause_Name (Clause));
         if Is_Nil (Size_Expr) then
            return;
         end if;
         declare
            S : constant Extended_Biggest_Natural := Discrete_Static_Expression_Value (Size_Expr);
         begin
            if S = Not_Static then
               Uncheckable (Rule_Id,
                            False_Negative,
                            Get_Location (Clause),
                            "unable to evaluate size of "
                            & Name_Image (Representation_Clause_Name (Clause))
                            & "for non_contiguous_layout subrule");
               return;
            end if;

            if Fields (Fields'Last).High /= S - 1 then
               Starting_Unit := (Fields (Fields'Last).High + 1) / Storage_Unit;
                Do_Report (Sr_Non_Contiguous_Layout,
                           Rep_Clause,
                          "gap at end of record, at "
                          & Biggest_Int_Img (Starting_Unit)
                          & " range "
                          & Biggest_Int_Img (Fields (Fields'Last).High + 1 - Starting_Unit * Storage_Unit)
                          & ".."
                          & Biggest_Int_Img (S - 1 - Starting_Unit * Storage_Unit)
                          & ", size clause line " & ASIS_Integer_Img (Get_First_Line (Get_Location (Size_Expr))),
                          Get_Location (Components (Fields (Fields'Last).Compo_Inx)));
            end if;
         end;
      end Check_Components;

      procedure Check_Overlay is
         Value : constant Asis.Expression := Ultimate_Expression (Representation_Clause_Expression (Rep_Clause));
      begin
         if Expression_Kind (Value) = An_Attribute_Reference
           and then Attribute_Kind (Value) = An_Address_Attribute
         then
            Do_Report (Sr_Overlay, Rep_Clause, "address clause causes overlay");
         end if;
      end Check_Overlay;

      procedure Check_Bit_Order is
         use Asis.Declarations;
         Rep_List  : constant Asis.Representation_Clause_List := Corresponding_Representation_Clauses
                                                                  (Corresponding_Name_Declaration
                                                                   (Simple_Name
                                                                    (Representation_Clause_Name (Rep_Clause))));
      begin
         for R in Rep_List'Range loop
            if Representation_Clause_Kind (Rep_List (R)) = An_Attribute_Definition_Clause
              and then Attribute_Kind (Representation_Clause_Name (Rep_List (R))) = A_Bit_Order_Attribute
            then
               return;
            end if;
         end loop;
         Do_Report (Sr_No_Bit_Order_Layout,
                    Rep_Clause,
                    "type in layout representation clause has no bit ordering specified",
                    Loc => Get_Location (Representation_Clause_Name (Rep_Clause)));

      end Check_Bit_Order;

   begin   -- Process_Clause
      if Rule_Used = Not_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Representation_Clause_Kind (Rep_Clause) is
         when Not_A_Representation_Clause =>
            Failure ("Not a representation clause in " & Rule_Id);

         when An_Attribute_Definition_Clause =>
            if not Rule_Used (Sr_Attribute)
              and not Rule_Used (Sr_Fractional_Size)
              and not Rule_Used (Sr_Overlay)
            then
               return;
            end if;

            Attribute := To_Unbounded_Wide_String (''' & To_Upper (Attribute_Name_Image
                                                                   (Representation_Clause_Name (Rep_Clause))));
            if Expression_Kind (Prefix (Representation_Clause_Name (Rep_Clause))) = An_Attribute_Reference then
               -- This happens only in for T'Class'Read and similar
               Attribute := "'CLASS" & Attribute;
            end if;

            Do_Report (Sr_Attribute, Rep_Clause, "representation clause for " & To_Wide_String (Attribute));

            if Attribute = "'SIZE" and then Rule_Used (Sr_Fractional_Size) then
               declare
                  Value : constant Extended_Biggest_Natural
                    := Discrete_Static_Expression_Value (Representation_Clause_Expression (Rep_Clause));
               begin
                  if Value = Not_Static then
                     Uncheckable (Rule_Id,
                                  False_Negative,
                                  Get_Location (Representation_Clause_Expression (Rep_Clause)),
                                  "unable to evaluate size for fractional_size subrule");
                  elsif Value rem Storage_Unit /= 0 then
                     Do_Report (Sr_Fractional_Size, Rep_Clause, "size clause not multiple of Storage_Unit");
                  end if;
               end;
            end if;

            if Attribute = "'ADDRESS" and then Rule_Used (Sr_Overlay) then
               Check_Overlay;
            end if;

         when An_Enumeration_Representation_Clause =>
            Do_Report (Sr_Enumeration, Rep_Clause, "enumeration representation clause");

         when A_Record_Representation_Clause =>
            Do_Report (Sr_Layout, Rep_Clause, "layout representation clause");

            if Rule_Used (Sr_At_Mod) and then not Is_Nil (Mod_Clause_Expression (Rep_Clause)) then
               Do_Report (Sr_At_Mod, Rep_Clause, "Ada 83 alignment clause");
            end if;

            if Rule_Used (Sr_Incomplete_Layout) then
               Check_Incomplete (Rep_Clause);
            end if;

            if Rule_Used (Sr_Non_Contiguous_Layout) or Rule_Used (Sr_Non_Aligned_Component) then
               Check_Components (Rep_Clause);
            end if;

            if Rule_Used (Sr_No_Bit_Order_Layout) then
               Check_Bit_Order;
            end if;

         when An_At_Clause =>
            Do_Report (Sr_At, Rep_Clause, "Ada 83 address clause");
            if Rule_Used (Sr_Overlay) then
               Check_Overlay;
            end if;
      end case;
   end Process_Clause;

begin  -- Rules.Representation_Clauses
   for K in Subrules range Subrules'Succ (Sr_Attribute) .. Subrules'Last loop
      Key (K) := Value (Subrules'Wide_Image (K));
   end loop;

   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Representation_Clauses;
