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
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
----------------------------------------------------------------------

-- Asis
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Control_Manager,
  Framework.Language,
  Framework.Language.Shared_Keys,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Derivations is
   use Framework, Framework.Control_Manager;

   -- Algorithm
   --
   -- From: we simply walk up the chain of derivations until we encounter something that's not a derived type.
   --
   -- TBSL: interfaces. Progenitors are ignored. Task and protected progenitors
   --       formal derived types, formal interfaces
   --       separate case of interfaces?
   --       subtypes?
   --       future subrules: Max_Depth, Max_Parents

   type Subrules is (SR_From, SR_Max_Parents);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules, Prefix => "SR_");
   type Subrules_Set is array (Subrules) of Boolean;
   No_Rule : constant Subrules_Set := (others => False);

   type Maxima is array (Control_Kinds) of Thick_Queries.Biggest_Natural;
   Uninitialized : constant Thick_Queries.Biggest_Natural := Thick_Queries.Biggest_Natural'Last;
   type Value_Context is new Basic_Rule_Context with
      record
         Maximum : Maxima := (others => Uninitialized);
      end record;

   Searched_Parents : Context_Store;

   Rule_Used : Subrules_Set := No_Rule;
   Save_Used : Subrules_Set;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls ancestors of a derived type");
      User_Message;
      User_Message ("Parameter(1): From | Max_Parents");
      User_Message ("for From subrule:");
      User_Message ("Parameter(2..): <Entity name> | <category>");
      User_Message ("<category>: ()      | access   | array | delta  | digits | mod |");
      User_Message ("            private |protected | range | record | tagged | task");
      User_Message ("for Max_Parents subrule:");
      User_Message ("Parameter(2)  : <value>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds)
   is
      use Framework.Language, Subrules_Flag_Utilities;

      Subrule_Name : Subrules;
      Bound        : Thick_Queries.Biggest_Natural;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "parameters required");
      end if;

      Subrule_Name := Get_Flag_Parameter (Allow_Any => False);
      case Subrule_Name is
         when SR_From =>
            loop
               declare
                  Entity : constant Entity_Specification := Get_Entity_Parameter (Allow_Extended => True);
               begin
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

   ----------------
   -- Check_From --
   ----------------

   ----------------
   -- Check_From --
   ----------------

   procedure Check_From (Element : Asis.Definition) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Language.Shared_Keys, Framework.Reports, Thick_Queries;

      function Derivation_Subtype_Name (Def : Asis.Definition) return Asis.Expression is
         use Asis.Definitions, Utilities;
      begin
         case Definition_Kind (Def) is
            when A_Type_Definition =>
               return Subtype_Simple_Name (Parent_Subtype_Indication (Def));
            when A_Formal_Type_Definition
               | A_Subtype_Indication
               =>
               return Subtype_Simple_Name (Def);
            when others =>
               Failure ("Derivation_Subtype_Name: bad definition_kind", Def);
         end case;
      end Derivation_Subtype_Name;

      --------------------
      -- Found_Ancestor --
      --------------------

      function Found_Ancestor (Elem : Asis.Element; Message : Wide_String) return Boolean is
         Context : constant Root_Context'Class := Matching_Context (Searched_Parents, Elem);
      begin
         if Context /= No_Matching_Context then
            Report (Rule_Id,
                    Context,
                    Get_Location (Element),
                    Message & ' ' & Last_Matching_Name (Searched_Parents));
            return True;
         end if;

         return False;
      end Found_Ancestor;

      Current          : Asis.Definition := Element;
      Current_Type_Def : Asis.Definition;
      Parent_Name      : Asis.Name;
      Parent_Decl      : Asis.Declaration;

   begin  -- Check_From
      loop
         Parent_Name := Derivation_Subtype_Name (Current);

         -- Check parent type
         if Found_Ancestor (Parent_Name, "Type derived from") then
            return;
         end if;

         Parent_Decl := Corresponding_Name_Declaration (Strip_Attributes (Parent_Name));

         -- If it is a subtype, check corresponding type
         if Declaration_Kind (Parent_Decl) not in A_Type_Declaration then
            if Found_Ancestor (Names (Corresponding_First_Subtype (Parent_Decl)) (1), "Type derived from") then
               return;
            end if;
         end if;

         -- Check enclosing unit of parent type
         if Found_Ancestor (Names (Unit_Declaration (Enclosing_Compilation_Unit (Parent_Decl))) (1),
                            "Type derived from type in unit")
         then
            return;
         end if;

         -- Move to upper parent
         Current          := Corresponding_Name_Declaration (Strip_Attributes (Parent_Name));
         Current_Type_Def := Type_Declaration_View (Corresponding_First_Subtype (Current));
         exit when Type_Kind (Current_Type_Def)
                   not in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
           and then Formal_Type_Kind (Current_Type_Def) /= A_Formal_Derived_Type_Definition;

         Current := Type_Declaration_View (Current);
      end loop;

      declare
         use Categories_Utilities;
         Context : constant Root_Context'Class
           := Control_Manager.Association (Searched_Parents,
                                           Image (Matching_Category (Element,
                                                                     From_Cats      => Full_Set,
                                                                     Follow_Derived => True,
                                                                     Privacy        => Follow_Private)));
      begin
         if Context /= No_Matching_Context then
            Report (Rule_Id,
                    Context,
                    Get_Location (Element),
                    "Type derived from category " & Last_Matching_Name (Searched_Parents));
         end if;
      end;
   end Check_From;

   -----------------------
   -- Check_Max_Parents --
   -----------------------

   procedure Check_Max_Parents (Element : Asis.Element; Value : Thick_Queries.Biggest_Natural) is
      use Framework.Reports, Thick_Queries;

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

   ------------------------
   -- Process_Derivation --
   ------------------------

   procedure Process_Derivation (Element :  Asis.Definition) is
      use Asis, Asis.Definitions, Asis.Elements;
      use type Thick_Queries.Biggest_Int;
   begin
      if Rule_Used = No_Rule then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (SR_From) then
         -- Interfaces have progenitors, but no parent...
         if Type_Kind (Element) /= An_Interface_Type_Definition then
            Check_From (Element);
         end if;
      end if;

      if Rule_Used (SR_Max_Parents) then
         -- Filter those who can have progenitors
         case Type_Kind (Element) is
            when A_Derived_Record_Extension_Definition
               | An_Interface_Type_Definition
                 =>
               Check_Max_Parents (Element, Definition_Interface_List (Element)'Length + 1);
            when others =>
               null;
         end case;

         case Formal_Type_Kind (Element) is
            when A_Formal_Derived_Type_Definition
               | A_Formal_Interface_Type_Definition
               =>
               Check_Max_Parents (Element, Definition_Interface_List (Element)'Length + 1);
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
   begin
      if Rule_Used = No_Rule then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Rule_Used (SR_Max_Parents) then
         Check_Max_Parents (Sync, Declaration_Interface_List (Sync)'Length);
      end if;
   end Process_Synchronized;

begin  -- Rules.Derivations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Derivations;